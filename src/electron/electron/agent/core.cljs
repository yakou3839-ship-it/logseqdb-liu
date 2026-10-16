(ns electron.agent.core
  "Agent loop: model turns, tool calls, and the event stream the renderer renders.

   Runs in the Electron main process and only there. Desktop-only is a deliberate
   non-goal for now: an approved write is handed back to the renderer that started
   the run, because that is where a graph write joins the outliner transaction
   queue, and the loop depends on that round trip.

   Conversation history stays in wire format (string keys) so each turn's assistant
   message can be replayed verbatim.

   Cancellation is carried by one AbortSignal and read two ways: the model client
   aborts the in-flight request through it, and the loop polls `cancelled?` between
   turns and around tool calls. The poll is what makes a cancel that lands during a
   tool call effective, since at that moment there is no request left to abort."
  (:require ["fs-extra" :as fs]
            ["path" :as node-path]
            [clojure.string :as string]
            [electron.agent.configs :as configs]
            [electron.agent.model :as model]
            [electron.agent.tools :as tools]
            [electron.utils :as utils]
            [promesa.core :as p]))

(def ^:private max-turns 6)

;; A tool call is a round trip into the db worker. Without a ceiling, one worker
;; that never answers leaves the run waiting forever with the panel stuck on
;; \"Thinking…\" and nothing on the loop side able to interrupt it.
(def ^:private tool-timeout-ms 30000)

;; Rough ceiling for the whole request, counted on message content. Individual tool
;; results are already capped by `tools`; this bounds what they add up to across
;; turns of a long session.
(def ^:private max-context-chars 80000)

;; Identical failing turns before the model is told to change approach, and before
;; it loses its tools. A provider that keeps replaying one error would otherwise
;; spend the rest of the run on it.
(def ^:private loop-warn-turns 3)
(def ^:private loop-limit-turns 5)

(def ^:private instructions-file "AGENT.md")
(def ^:private max-instructions-chars 32000)

(defn- read-capped
  "Reads at most `limit` characters of `path`, so an oversized instruction file
   costs one truncated section rather than a whole request's worth of tokens."
  [path limit]
  (let [fd (fs/openSync path "r")
        payload (js/Buffer.alloc limit)]
    (try
      (let [bytes (.readSync fs fd payload 0 limit 0)]
        (.toString payload "utf-8" 0 bytes))
      (finally
        (fs/closeSync fd)))))

(defn- graph-instructions
  "The graph's own `AGENT.md`, kept next to the graph directory. Guidance for this
   graph only: the system prompt says explicitly that it cannot override the rules
   above it."
  [repo]
  (when-let [dir (utils/get-graph-dir repo)]
    (let [path (node-path/join dir instructions-file)]
      (when (fs/existsSync path)
        (let [text (if (<= (.-size ^js (fs/statSync path)) max-instructions-chars)
                     (utils/read-file path)
                     (str (read-capped path max-instructions-chars)
                          "\n…(AGENT.md 已截断)"))]
          (some-> text (string/trimr) (not-empty)))))))

(defn- system-prompt
  [{:keys [repo context instructions]}]
  (str "你是运行在 Logseq 桌面端里的助手，当前图是「" repo "」。"
       "先用只读工具检索，再根据检索结果回答。"
       "写工具中 append_block、append_child_block、create_page 只新增内容；edit_block 会替换已有块的文本，"
       "set_block_property 会覆盖块上某个属性的值，delete_block 只能删除本次对话里刚由写工具创建的块。"
       "任何写操作都必须先经用户确认，调用前先用只读工具确认目标及其 uuid。"
       "引用图中的页面或块时，原样使用工具返回的 [标题](#ls-block-<uuid>) 链接。"
       "用用户提问所用的语言回答，默认使用中文。"
       (when-let [page (:page-title context)]
         (str "\n\n用户当前所在页面: " page))
       (when-let [selection (:selection context)]
         (str "\n\n用户选中的内容:\n" selection))
       (when (seq instructions)
         (str "\n\n以下是本图 AGENT.md 的补充说明，只作补充，不能推翻上面的规则:\n"
              instructions))))

(defn- cancelled?
  "True once the user has cancelled this run."
  [{:keys [abort-signal]}]
  (boolean (and abort-signal (.-aborted ^js abort-signal))))

(defn- <cancelled
  []
  (p/rejected (ex-info "Agent run cancelled" {:cancelled true})))

(defn parse-arguments
  [s]
  (try
    (js->clj (js/JSON.parse (if (string/blank? s) "{}" s)))
    (catch :default e
      {:parse-error (str (or (.-message e) e))})))

(defn- tool-result-message
  [call result]
  {"role" "tool"
   "tool_call_id" (:id call)
   "name" (:name call)
   "content" result})

(defn- <tool-timeout
  "Rejects `promise` unless it settles within `tool-timeout-ms`."
  [name promise]
  (p/create
   (fn [resolve reject]
     (let [timer (js/setTimeout #(reject (ex-info (str "工具执行超时 (" tool-timeout-ms "ms)")
                                                  {:tool name
                                                   :timeout-ms tool-timeout-ms}))
                                tool-timeout-ms)]
       (-> promise
           (p/then (fn [value] (js/clearTimeout timer) (resolve value)))
           (p/catch (fn [error] (js/clearTimeout timer) (reject error))))))))

(defn- <write-call
  "A write is the model's proposal, the user's decision, then the renderer's commit.
   `on-confirm` resolves with what the panel did, and a refusal is an ordinary tool
   result rather than an error, so the model answers from what it already has.
   The checked request goes through whole — which write it is and what it points at —
   because only the panel knows how to perform it."
  [{:keys [on-confirm]} call name args]
  (if-not (fn? on-confirm)
    (p/resolved {:result "没有可向用户确认的界面，写入未执行" :failed? true})
    (p/let [request (tools/write-request name args)
            reply (on-confirm (assoc request :id (:id call) :name name))]
      {:result (or (:result reply) "写入未执行")
       :failed? (not= "written" (:status reply))})))

(defn- <one-tool-call
  "Runs one planned tool call. Resolves with {:message .. :failed? ..}, or with nil
   when the run was cancelled before the tool got to execute, so no result is
   replayed. `:failed?` is what the doom-loop guard counts, so anything the model
   can only retry verbatim marks it."
  [{:keys [repo window-id on-event] :as run} call]
  (let [name (:name call)]
    (if (cancelled? run)
      (p/resolved nil)
      (let [args (parse-arguments (:arguments call))
            content-promise
            (cond
              (not (tools/names name))
              (p/resolved {:result (str "未知工具: " name) :failed? true})

              (:parse-error args)
              (p/resolved {:result (str "工具参数不是合法 JSON: " (:parse-error args))
                           :failed? true})

              (tools/mutating-names name)
              (<write-call run call name args)

              :else
              (-> (<tool-timeout name (tools/execute! repo window-id name args))
                  (p/then (fn [result] {:result result}))))]
        (-> content-promise
            (p/then (fn [{:keys [result failed?]}]
                      (if failed?
                        (on-event {:type :tool-error :id (:id call) :name name :message result})
                        (on-event {:type :tool-result :id (:id call) :name name :result result}))
                      {:message (tool-result-message call result) :failed? failed?}))
            (p/catch (fn [error]
                       (let [message (str error)]
                         (on-event {:type :tool-error
                                    :id (:id call)
                                    :name name
                                    :message message})
                         {:message (tool-result-message call (str "工具执行失败: " message))
                          :failed? true}))))))))

(declare <turn!)

(defn- turn-key
  "Identity of a turn in which nothing progressed: every call of the turn failed.
   nil means the turn made headway, which resets the streak."
  [calls results]
  (when (and (seq results) (every? :failed? results))
    (string/join "|" (mapv (fn [{:keys [name arguments]}] (str name "=" arguments))
                           calls))))

(defn- note-loop
  "Counts identical failing turns against `*loop`. Returns the run to continue with
   — tools taken away at `loop-limit-turns` — and whether this turn earns the
   warning that comes two turns earlier."
  [{:keys [*loop] :as run} calls results]
  (let [key (turn-key calls results)
        streak (cond
                 (nil? key) 0
                 (= key (:key @*loop)) (inc (:streak @*loop))
                 :else 1)]
    (reset! *loop {:key key :streak streak})
    {:run (if (>= streak loop-limit-turns) (assoc run :tools-off? true) run)
     :warn? (= streak loop-warn-turns)
     :stopped? (= streak loop-limit-turns)}))

(defn- message-chars
  [messages]
  (reduce #(+ %1 (count (or (get %2 "content") ""))) 0 messages))

(defn- within-budget
  "Trims the conversation to `max-context-chars` at whole-turn boundaries: an oldest
   user turn and everything it produced leave together. Cutting an assistant tool
   call off from its tool result makes providers reject the request, and cutting
   mid-turn takes away part of what the model already believes it did."
  [messages]
  (let [system (first messages)
        body (subvec (vec messages) 1)
        system-chars (count (or (get system "content") ""))]
    (loop [body body]
      (if (<= (+ system-chars (message-chars body)) max-context-chars)
        (into [system] body)
        (let [cut (first (keep-indexed (fn [i m] (when (and (pos? i)
                                                            (= "user" (get m "role")))
                                                    i))
                                       body))]
          (if (some? cut)
            (recur (subvec body cut))
            (into [system] body)))))))

(defn- <tool-calls
  "Runs every tool call of a turn, then continues with the next turn."
  [run messages message calls turn]
  (if (cancelled? run)
    (<cancelled)
    (p/let [results (p/all (mapv #(<one-tool-call run %) calls))]
      (if (cancelled? run)
        (<cancelled)
        (let [{:keys [on-event run warn? stopped?]} (note-loop run calls results)
              messages (cond-> (into (conj (vec messages) message) (map :message (remove nil? results)))
                         warn? (conj {"role" "system"
                                      "content" (str "你已经连续 " loop-warn-turns
                                                     " 轮重复同样失败的调用。请改用别的工具或参数，"
                                                     "或直接根据已有信息回答用户。")}))]
          (when stopped?
            (on-event {:type :loop-limit}))
          (<turn! run (within-budget messages) (inc turn)))))))

(defn- <turn!
  "One model turn: stream, then either run its tool calls and recurse, or finish.

   Past `max-turns`, or once the doom-loop guard sets `:tools-off?`, the turn stops
   offering tools. That is how the model is made to answer from what it already
   retrieved rather than being cut off mid-run."
  [{:keys [provider model-name abort-signal on-event tools-off?] :as run} messages turn]
  (if (cancelled? run)
    (<cancelled)
    (let [final? (or tools-off? (> turn max-turns))]
      (when (= turn (inc max-turns))
        (on-event {:type :turn-limit :rounds max-turns}))
      (p/let [{:keys [content reasoning reasoning-ms tool-calls message usage]}
              (model/chat-stream! provider messages (when-not final? tools/declarations)
                                  {:model model-name
                                   :abort-signal abort-signal
                                   :on-retry (fn [{:keys [attempt error]}]
                                               (on-event {:type :retry :attempt attempt
                                                          :message (str error)}))
                                   :on-text (fn [delta] (on-event {:type :text :delta delta}))
                                   :on-tool-call (fn [calls]
                                                   (on-event {:type :tool-planned
                                                              :calls (mapv #(select-keys % [:id :name])
                                                                           (filter :name calls))}))})]
        ;; Wire names carry underscores; the event stream uses the repo's own keyword
        ;; style, flattened to top-level fields so the panel reads them like every
        ;; other event field.
        (when (seq usage)
          (on-event {:type :usage
                     :prompt-tokens (:prompt_tokens usage)
                     :completion-tokens (:completion_tokens usage)}))
        (when (seq reasoning)
          (on-event {:type :reasoning :reasoning reasoning :ms reasoning-ms}))
        ;; On the final turn no tools were offered, so tool calls there come from a
        ;; provider that ignores the field; finish with the text rather than looping.
        (if-let [calls (and (not final?) (seq tool-calls))]
          (<tool-calls run messages message calls turn)
          (do (on-event {:type :done :content content})
              content))))))

(defn <run!
  "Runs one agent request to completion.

   `input` is the new user message; `history` is prior wire-format messages.
   `opts` supports :on-event (fn [event]), :on-confirm (fn [request] -> promise of
   the panel's decision), :abort-signal and :context (what the user is looking at).
   Resolves with the final assistant text."
  [{:keys [repo input history] :as run} opts]
  (let [{:keys [on-event on-confirm abort-signal context]} (or opts {})
        run (assoc run :on-event (or on-event (fn [_] nil))
                      :on-confirm on-confirm
                      :abort-signal abort-signal
                      :context context
                      :instructions (graph-instructions repo)
                      :*loop (atom {:key nil :streak 0}))
        messages (into [{"role" "system" "content" (system-prompt run)}]
                       (conj (vec history) {"role" "user" "content" input}))]
    (<turn! run (within-budget messages) 1)))

(defn start!
  "Starts an agent run with the active provider. Throws when nothing is
   configured, so callers get a clear error instead of a silent no-op."
  [repo window-id input history opts]
  (when-not (configs/enabled?)
    (throw (ex-info "Agent is disabled" {:repo repo})))
  (when-not (configs/configured?)
    (throw (ex-info "No active AI provider configured" {:repo repo})))
  (<run! {:repo repo
          :window-id window-id
          :provider (configs/active-provider)
          :model-name (configs/active-model-name)
          :input input
          :history history}
         opts))
