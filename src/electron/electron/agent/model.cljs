(ns electron.agent.model
  "OpenAI-compatible chat/completions streaming client for the Electron main process.

   Conversation history is kept in wire format (string keys) so an assistant turn
   can be appended to the next request without reshaping it."
  (:require [clojure.string :as string]
            [electron.logger :as logger]
            [electron.utils :as utils]
            [promesa.core :as p]))

(defn- endpoint
  [base-url]
  (str (string/replace base-url #"/+$" "") "/chat/completions"))

(defn- parse-json
  [s]
  (js->clj (js/JSON.parse s) :keywordize-keys true))

(defn- frame-payload
  "Joins the data: lines of one SSE frame; nil for comments and keep-alives."
  [frame]
  (let [lines (->> (string/split-lines frame)
                   (filter #(string/starts-with? % "data:"))
                   (map #(string/trim (subs % 5))))]
    (when (seq lines)
      (string/join "\n" lines))))

(defn- merge-tool-call
  [existing delta]
  (let [{:keys [id index]} delta
        {:keys [name arguments]} (:function delta)]
    (cond-> (or existing {})
      (some? id) (assoc :id id)
      (some? index) (assoc :index index)
      (some? name) (assoc :name name)
      (some? arguments) (update :arguments (fnil str "") arguments))))

(defn- close-thinking!
  "Ends the open thinking window, if any. The window opens on the first streamed
   delta and closes as soon as the model emits real output, which is what a reader
   perceives as \"thinking\"."
  [{:keys [thinking-start-ref thinking-ms-ref]}]
  (when-let [start @thinking-start-ref]
    (reset! thinking-start-ref nil)
    (swap! thinking-ms-ref + (- (js/Date.now) start)))
  nil)

(defn- tool-calls-seq
  "The calls accumulated so far, in provider index order. Providers stream the
   deltas of parallel calls interleaved, so the index is what keeps them apart."
  [tool-calls-ref]
  (mapv val (into (sorted-map) @tool-calls-ref)))

(defn- decode-payload!
  "Applies one SSE data payload to the run state. Returns true once the stream is done."
  [{:keys [content-ref reasoning-ref thinking-start-ref thinking-ms-ref
           tool-calls-ref finish-ref usage-ref on-text on-tool-call] :as state} payload]
  (if (= payload "[DONE]")
    true
    (let [{:keys [choices error usage]} (parse-json payload)]
      (when usage
        ;; With `stream_options.include_usage` the usage arrives as its own final
        ;; frame with empty `choices`, after the answer has already streamed.
        (reset! usage-ref usage))
      (when error
        (throw (ex-info "Model returned an error payload" {:error error})))
      (when-let [{:keys [delta finish_reason]} (first choices)]
        (when (nil? @thinking-start-ref)
          (reset! thinking-start-ref (js/Date.now)))
        (let [text       (and (string? (:content delta)) (:content delta))
              reasoning  (and (string? (:reasoning_content delta))
                              (:reasoning_content delta))
              tool-calls (:tool_calls delta)]
          (when text
            (swap! content-ref str text)
            (when (and on-text (pos? (count text)))
              (on-text text)))
          ;; Thinking models reject a follow-up request whose assistant message omits
          ;; the reasoning text, so keep it for replay.
          (when reasoning
            (swap! reasoning-ref str reasoning))
          (when (or (and text (pos? (count text))) (seq tool-calls))
            (close-thinking! state))
          (doseq [tc tool-calls]
            (swap! tool-calls-ref update (:index tc) #(merge-tool-call % tc))
            (when (and on-tool-call (some? (:name (:function tc))))
              (on-tool-call (tool-calls-seq tool-calls-ref)))))
        (when finish_reason
          (reset! finish-ref finish_reason)))
      false)))

(defn- take-frames
  "Returns [complete-frames remainder]. cljs.string/split drops the empty segment
   after a terminal \\n\\n, which would strand the last frame in the buffer and
   merge it into the next chunk."
  [text]
  (loop [s text
         frames []]
    (if-let [sep (string/index-of s "\n\n")]
      (recur (subs s (+ sep 2)) (conj frames (subs s 0 sep)))
      [frames s])))

(defn- consume-chunk!
  [{:keys [buf-ref decoder done-ref arm-idle!] :as state} ^js chunk]
  (when-not @done-ref
    (arm-idle!)
    (let [text (str @buf-ref (.decode decoder chunk #js {:stream true}))
          [frames remainder] (take-frames (string/replace text #"\r\n" "\n"))]
      (reset! buf-ref remainder)
      (doseq [frame frames]
        (when-not @done-ref
          (when-let [payload (frame-payload frame)]
            (reset! done-ref (decode-payload! state payload))))))))

(defn- <read-stream!
  "Resolves with the run state; the stream can also be destroyed from the outside
   (abort), which surfaces as an error on the fetch promise instead."
  [^js stream state]
  (p/create
   (fn [resolve reject]
     (.on stream "data"
          (fn [^js chunk]
            (try
              (consume-chunk! state chunk)
              (catch :default e
                (.destroy stream)
                (reject e)))))
     (.on stream "end" (fn [] (resolve state)))
     (.on stream "error" (fn [^js e] (reject e))))))

(defn- request-body
  [model messages tools]
  (let [body #js {"model" model
                  "messages" (clj->js messages)
                  "stream" true
                  ;; The only documented way to get token accounting on a stream:
                  ;; usage then arrives as a final frame with empty choices.
                  "stream_options" #js {"include_usage" true}}]
    (when (seq tools)
      (aset body "tools" (clj->js tools)))
    body))

(defn- wire-tool-calls
  [tool-calls]
  (mapv (fn [{:keys [index id name arguments]}]
          #js {"id" id
               "type" "function"
               "index" index
               "function" #js {"name" name
                               "arguments" (or arguments "")}})
        tool-calls))

(defn- assistant-message
  [content reasoning tool-calls]
  (let [message #js {"role" "assistant"}]
    (when (seq content)
      (aset message "content" content))
    (when (seq reasoning)
      (aset message "reasoning_content" reasoning))
    (when (seq tool-calls)
      ;; A raw cljs vector would serialize its internals (meta/cnt/root/tail),
      ;; which strict OpenAI-compatible providers reject with a 422.
      (aset message "tool_calls" (clj->js (wire-tool-calls tool-calls))))
    message))

(defn- idle-timeout-ms
  [provider]
  (* 1000 (or (:request-timeout provider) 120)))

(def ^:private retry-attempts 3)
(def ^:private retry-base-ms 1000)
(def ^:private retry-cap-ms 64000)

(defn- aborted?
  [{:keys [abort-signal]}]
  (boolean (and abort-signal (.-aborted ^js abort-signal))))

(defn- retryable-error?
  "Transient failures only: a dropped connection, a rate limit, or a server error.
   A 4xx means the request itself is wrong, so sending it again only repeats the
   answer. An abort is never retryable, or pressing stop would restart the turn."
  [error]
  (if-let [status (:status (ex-data error))]
    (or (= 429 status) (<= 500 status))
    (not= "AbortError" (.-name error))))

(defn- backoff-ms
  "Exponential with a cap, plus jitter so concurrent clients do not retry in step."
  [attempt]
  (let [base (min retry-cap-ms (* retry-base-ms (bit-shift-left 1 (dec attempt))))]
    (+ base (js/Math.round (* 0.2 base (js/Math.random))))))

(defn- <sleep
  [ms]
  (js/Promise. (fn [resolve] (js/setTimeout resolve ms))))

(defn- <one-attempt!
  "One streaming request. Owns its AbortController and idle timer, so a retry starts
   from a clean slate instead of a controller the previous attempt already aborted."
  [provider messages tools {:keys [model idle-ms on-text on-tool-call abort-signal]}]
  (let [controller (js/AbortController.)
        idle-timer-ref (atom nil)
        arm-idle! (fn []
                    (when-let [^js timer @idle-timer-ref]
                      (js/clearTimeout timer))
                    (reset! idle-timer-ref (js/setTimeout #(.abort controller) idle-ms)))
        abort-handler #(.abort controller)
        state {:content-ref (atom "")
               :reasoning-ref (atom "")
               :thinking-start-ref (atom nil)
               :thinking-ms-ref (atom 0)
               :tool-calls-ref (atom {})
               :finish-ref (atom nil)
               :usage-ref (atom nil)
               :done-ref (atom false)
               :buf-ref (atom "")
               :decoder (js/TextDecoder. "utf-8")
               :arm-idle! arm-idle!
               :on-text on-text
               :on-tool-call on-tool-call}]
    (when abort-signal
      (.addEventListener ^js abort-signal "abort" abort-handler)
      ;; A signal that is already aborted never fires its "abort" event, so the
      ;; request would otherwise go out after the user pressed stop.
      (when (.-aborted ^js abort-signal)
        (abort-handler)))
    (arm-idle!)
    (-> (p/let [request-json (js/JSON.stringify (request-body model messages tools))
                ^js res (utils/fetch (endpoint (:base-url provider))
                                     {:method "POST"
                                      :headers #js {"content-type" "application/json"
                                                    "accept" "text/event-stream"
                                                    "authorization" (str "Bearer " (:api-key provider))}
                                      :body request-json
                                      :signal (.-signal controller)})
                _ (arm-idle!)
                _ (when-not (.-ok res)
                    (p/let [^js payload (.text res)
                            response-body (str payload)]
                      ;; Log the full request to main.log so protocol mismatches
                      ;; with picky OpenAI-compatible providers can be diagnosed.
                      (logger/error "agent model request failed"
                                    {:status (.-status res)
                                     :request request-json
                                     :response (subs response-body 0 (min (count response-body) 500))})
                      ;; The body rides inside the error across IPC and is rendered
                      ;; in the chat panel, so keep it short.
                      (throw (ex-info "Model request failed"
                                      {:status (.-status res)
                                       :body (subs response-body 0 (min (count response-body) 500))}))))]
         (<read-stream! (.-body res) state))
        (p/then (fn [{:keys [content-ref reasoning-ref thinking-ms-ref
                             tool-calls-ref finish-ref usage-ref] :as state}]
                  (close-thinking! state)
                  (let [content    @content-ref
                        reasoning  @reasoning-ref
                        tool-calls (tool-calls-seq tool-calls-ref)]
                    {:content content
                     :reasoning reasoning
                     :reasoning-ms @thinking-ms-ref
                     :tool-calls tool-calls
                     :usage @usage-ref
                     :finish-reason (keyword @finish-ref)
                     :message (assistant-message content reasoning tool-calls)})))
        (p/finally (fn [_]
                     (when-let [^js timer @idle-timer-ref]
                       (js/clearTimeout timer))
                     (when abort-signal
                       (.removeEventListener ^js abort-signal "abort" abort-handler)))))))

(defn chat-stream!
  "Runs one streaming model turn, retrying transient request failures.
   `provider` comes from electron.agent.configs; `opts` supports :model, :on-text,
   :on-tool-call, :on-retry and :abort-signal.

   `:request-timeout` bounds how long the client waits for the *next* byte, not
   the whole exchange: connect, first byte, and every later gap share one idle
   window that each chunk restarts, so a slow but progressing answer is never cut off.

   Resolves with {:message <wire assistant message> :content <string>
                   :reasoning <string> :reasoning-ms <int> :usage <map|nil>
                   :tool-calls [{:index :id :name :arguments}] :finish-reason kw}.
   `:message` carries `reasoning_content` when the model thought, so the next turn
   of a thinking model replays legally."
  ([provider messages tools]
   (chat-stream! provider messages tools nil))
  ([provider messages tools opts]
   (let [attempt-opts (assoc (or opts {})
                             :model (or (:model opts) (first (:models provider)))
                             :idle-ms (idle-timeout-ms provider))]
     (letfn [(attempt [n]
               (-> (<one-attempt! provider messages tools attempt-opts)
                   (p/catch (fn [error]
                              (cond
                                (aborted? attempt-opts)
                                (p/rejected error)

                                (and (< n retry-attempts) (retryable-error? error))
                                (do (logger/warn "agent model request retried"
                                                 {:attempt (inc n) :error (str error)})
                                    (when-let [on-retry (:on-retry attempt-opts)]
                                      (on-retry {:attempt (inc n) :error (str error)}))
                                    (-> (<sleep (backoff-ms n))
                                        (p/then (fn [_]
                                                  (if (aborted? attempt-opts)
                                                    (p/rejected error)
                                                    (attempt (inc n)))))))

                                :else
                                (p/rejected error))))))]
       (attempt 1)))))
