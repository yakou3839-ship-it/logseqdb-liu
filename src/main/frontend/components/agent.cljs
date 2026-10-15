(ns frontend.components.agent
  "Agent panel: a standalone rightmost column with the chat UI and the event
   stream from the main process. Conversations are multi-session and persist
   out of the graph (see `electron.agent.session`); the panel restores the most
   recent session on mount. Sits next to the right sidebar and toggles
   independently of it."
  (:require ["react" :as react]
            [cljs-bean.core :as bean]
            [clojure.string :as string]
            [electron.ipc :as ipc]
            [frontend.context.i18n :refer [t]]
            [frontend.handler.editor :as editor-handler]
            [frontend.handler.plugin :as plugin-handler]
            [frontend.handler.property :as property-handler]
            [frontend.handler.route :as route-handler]
            [frontend.handler.ui :as ui-handler]
            [frontend.modules.outliner.op :as outliner-op]
            [frontend.modules.outliner.ui :as ui-outliner-tx]
            [frontend.rfx :as rfx]
            [frontend.security :as security]
            [frontend.state :as state]
            [frontend.ui :as ui]
            [frontend.util :as util]
            [frontend.util.entity :as entity]
            [io.factorhouse.hsx.core :as hsx]
            [logseq.shui.hooks :as hooks]
            [logseq.shui.ui :as shui]
            [promesa.core :as p]))

(def ^:private dialog-roles #{"user" "assistant"})
(def ^:private max-history-chars 24000)
(def ^:private max-title-chars 40)

;; Deltas arrive per token, which is far more often than the panel needs to paint.
(def ^:private delta-flush-ms 60)

;; How close to the bottom still counts as "watching the answer arrive"; past it the
;; user has scrolled away on purpose and nothing may yank them back.
(def ^:private bottom-threshold-px 48)

(defn- wire-history
  "Replays only complete dialog entries, most recent turns first within a char
   budget, so history cannot grow without bound across a long session."
  [entries]
  (let [dialog (filter #(and (dialog-roles (:role %)) (pos? (count (:text %)))) entries)
        within (reverse
                (second
                 (reduce (fn [[budget acc] entry]
                           (let [cost (count (:text entry))]
                             (if (> cost budget)
                               [budget acc]
                               [(- budget cost) (conj acc entry)])))
                         [max-history-chars []]
                         (reverse dialog))))
        ;; Never start the replay on an assistant turn.
        trimmed (drop-while #(= "assistant" (:role %)) within)]
    (mapv (fn [{:keys [role text]}]
            #js {"role" role "content" text})
          trimmed)))

(defn- storable-entries
  "Keeps complete user→assistant pairs only. A user turn that never got an
   answer (failed/cancelled run) stays visible in the UI but never reaches
   disk, so a restored session can never begin with orphan questions."
  [entries]
  (loop [pending-user nil
         pair-open? false
         [entry & more] entries
         out []]
    (if-not entry
      out
      (case (:role entry)
        "user" (recur entry false more out)
        "assistant" (if (seq (:text entry))
                      (cond pending-user
                            (recur nil true more (conj out pending-user (select-keys entry [:role :text])))
                            pair-open?
                            (recur nil true more (conj out (select-keys entry [:role :text])))
                            :else
                            (recur nil false more out))
                      (recur pending-user pair-open? more out))
        (recur pending-user pair-open? more out)))))

(defn- ->title
  [text]
  (let [text (string/trim text)]
    (if (> (count text) max-title-chars)
      (str (subs text 0 max-title-chars) "…")
      text)))

(defn- append-text!
  [entries* delta]
  (swap! entries*
         (fn [entries]
           (if (= "assistant" (:role (last entries)))
             (update-in entries [(dec (count entries))]
                        ;; Streaming resumed on a turn that had already been marked
                        ;; finished, so the markdown rendered from the older text no
                        ;; longer describes it.
                        (fn [entry] (-> entry
                                        (update :text str delta)
                                        (dissoc :rich? :html))))
             (conj entries {:role "assistant" :text delta}))))
  nil)

(defn- append-reasoning!
  [entries* reasoning ms]
  (swap! entries*
         (fn [entries]
           (if (= "assistant" (:role (last entries)))
             (update-in entries [(dec (count entries))]
                        (fn [entry]
                          (-> entry
                              (update :reasoning (fnil str "") reasoning)
                              (update :thinking-ms (fnil + 0) ms))))
             (conj entries {:role "assistant" :text ""
                            :reasoning reasoning :thinking-ms ms}))))
  nil)

(defn- step-index
  [entries step-key]
  (first (keep-indexed (fn [idx entry]
                         (when (and (= "tool" (:role entry))
                                    (= (:step-key entry) step-key))
                           idx))
                       entries)))

(defn- record-step!
  "Creates or updates the row tracking one tool call. Rows are keyed by the call id
   (the tool name when a provider streams none) so parallel calls of the same tool
   keep their own row and state instead of one row flip-flopping. Steps are live
   progress only: `storable-entries` keeps them out of the saved session."
  [entries* step-key name state patch]
  (swap! entries*
         (fn [entries]
           (if-let [idx (step-index entries step-key)]
             (update-in entries [idx] merge (assoc patch :state state))
             (conj entries (merge {:role "tool" :step-key step-key :name name :state state}
                                  patch))))))

(defn- flush-deltas!
  "Applies whatever streaming text is buffered. Called on the timer and before any
   event that appends its own entry, so a step row or a system line never lands
   above text that was still in the buffer."
  [{:keys [deltas* timer* entries*]}]
  (when-let [^js timer @timer*]
    (js/clearTimeout timer)
    (reset! timer* nil))
  (when-let [{:keys [kind text ms]} @deltas*]
    (reset! deltas* nil)
    (case kind
      :text (append-text! entries* text)
      :reasoning (append-reasoning! entries* text ms))))

(defn- queue-delta!
  [deps kind delta ms]
  (let [{:keys [deltas* timer*]} deps]
    (when (and @deltas* (not= kind (:kind @deltas*)))
      (flush-deltas! deps))
    (swap! deltas* (fn [{old-text :text old-ms :ms}]
                     {:kind kind
                      :text (str (or old-text "") delta)
                      :ms (+ (or old-ms 0) (or ms 0))}))
    (when-not @timer*
      (reset! timer* (js/setTimeout #(do (reset! timer* nil)
                                         (flush-deltas! deps))
                                     delta-flush-ms)))))

(defn- markdown-html
  "Renders a finished assistant turn through the markdown parser the app already
   ships, then sanitizes it. Streaming turns stay plain text: a half-typed link
   would render as garbage."
  [text]
  (some-> text not-empty plugin-handler/markdown-to-html security/sanitize-html))

(def ^:private citation-href #"#?ls-block-([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})")

(defn- citation-uuid
  "Recovers the block or page uuid from a tool citation (`[标题](#ls-block-<uuid>)`)
   once markdown turned it into an href."
  [href]
  (when href
    (some-> (re-find citation-href href) second uuid)))

(defn- <jump-to-citation!
  [id]
  (when-let [repo (state/get-current-repo)]
    (p/let [entity (state/<invoke-db-worker :thread-api/pull repo
                                            [:db/id {:block/page [:block/uuid]}]
                                            [:block/uuid id])]
      (when (:db/id entity)
        (if-let [page-uuid (get-in entity [:block/page :block/uuid])]
          (route-handler/redirect-to-page! page-uuid {:anchor (str "ls-block-" id)})
          (route-handler/redirect-to-page! id))))))

(defn- on-citation-click!
  [^js e]
  (when-let [^js anchor (.closest (.-target e) "a")]
    (when-let [id (citation-uuid (.getAttribute anchor "href"))]
      (.preventDefault e)
      (<jump-to-citation! id))))

(def ^:private target-attrs
  "What the panel reads about a write's target: its text for the confirmation, its tags
   for telling a page from a block, and its page for the outliner transaction."
  [:db/id :block/title {:block/tags [:db/ident]} {:block/page [:block/uuid]}])

(def ^:private direct-ops
  "The writes that only ever add content, and so can be allowed once for the rest of a
   conversation. Anything that changes or removes existing content is confirmed every
   time, whatever was allowed before."
  #{"append-block" "append-child-block" "create-page"})

(def ^:private max-target-chars 80)

(defn- <pull-by-uuid
  [id attrs]
  (state/<invoke-db-worker :thread-api/pull (state/get-current-repo)
                           attrs [:block/uuid (uuid id)]))

(defn- short-title
  "A target named inside a sentence is an excerpt; its full text is shown under it."
  [title]
  (let [title (or title "")]
    (if (> (count title) max-target-chars)
      (str (subs title 0 max-target-chars) "…")
      title)))

(defn- citation
  "What a finished write hands back to the model, in the same form the read tools use,
   so it can quote what it just wrote the way it quotes the rest of the graph."
  [title id]
  (str "[" (or title "") "](#ls-block-" (str id) ")"))

(defn- action-text
  "The pending write as one translated sentence naming its target: shown in the
   confirmation, and repeated in the conversation once the write lands."
  [{:keys [op target-title]}]
  (let [target (short-title target-title)]
    (case op
      "append-block" (t :agent.write/append-block-desc target)
      "append-child-block" (t :agent.write/append-child-block-desc target)
      "create-page" (t :agent.write/create-page-desc target)
      "edit-block" (t :agent.write/edit-block-desc target)
      "set-property" (t :agent.write/set-property-desc target)
      "delete-block" (t :agent.write/delete-block-desc target))))

(defn- <target
  "The node a proposed write points at, or nil when the graph no longer has it. The
   outliner operations do nothing at all for a target that cannot be found, so this
   pull is the only place that can tell a stale citation from a finished write."
  [target-id]
  (p/let [entity (<pull-by-uuid target-id target-attrs)]
    (when (:db/id entity)
      {:target-title (:block/title entity)
       :page-uuid (or (get-in entity [:block/page :block/uuid]) (uuid target-id))
       :page? (entity/page? entity)})))

(defn- write-problem
  "Why a proposal cannot be carried out as it stands, as the sentence the user reads and
   the one the model gets back. nil means the panel can go ahead with it."
  [{:keys [op target-id property-id property-ident]} target created-ids*]
  (cond
    (nil? target)
    {:text (t :agent.write/missing-target-error target-id)
     :result "写入目标不存在，未做任何改动"}

    (= "append-block" op)
    (when-not (:page? target)
      {:text (t :agent.write/wrong-target-error target-id)
       :result "append_block 的 page-id 必须是页面的 uuid；要往某个块下面加子块，请改用 append_child_block"})

    (= "edit-block" op)
    (when (:page? target)
      {:text (t :agent.write/wrong-target-error target-id)
       :result "edit_block 只改块的文字，页面标题不在它的范围内"})

    (= "delete-block" op)
    (when-not (contains? @created-ids* (uuid target-id))
      {:text (t :agent.write/not-created-error target-id)
       :result "delete_block 只能删除本次对话中由写工具创建的块，图里原有的内容不会被删除"})

    (= "set-property" op)
    (when-not property-ident
      {:text (t :agent.write/missing-target-error property-id)
       :result "该属性不存在，未做任何改动；请先用 list_properties 取到属性的 uuid"})))

(defn- <prepare-request
  "Reads in everything the panel needs to show a write — or to refuse it without
   bothering the user, when the uuid the model copied is gone, is the wrong kind of
   node, or is content the agent has no business deleting."
  [{:keys [op] :as request} created-ids*]
  (if-not (:target-id request)
    (p/resolved (assoc request :target-title (:page-title request)))
    (p/let [target (<target (:target-id request))
            property (when (= "set-property" op)
                       (<pull-by-uuid (:property-id request) [:db/ident :block/title]))
            request (-> request
                        (merge target)
                        (assoc :property-ident (:db/ident property)
                               :property-title (:block/title property)
                               ;; A page is named by its title alone; a block's own text
                               ;; is the content the write works on, so show it whole.
                               :current-text (when-not (:page? target)
                                               (:target-title target))))]
      (if-let [problem (write-problem request target created-ids*)]
        (assoc request :error problem)
        request))))

(defn- <append-block!
  "One new block at the bottom of the target — a page for `append_block`, another block
   for `append_child_block`, which is the same outliner operation. Its uuid is minted
   here and kept, which is what lets `delete_block` later recognize the block as one
   this conversation wrote."
  [{:keys [created-ids*]} {:keys [target-id page-uuid content]}]
  (let [block-uuid (random-uuid)]
    (p/let [_ (ui-outliner-tx/transact!
                {:outliner-op :insert-blocks :ui/page-id page-uuid}
                (outliner-op/insert-blocks! [{:block/uuid block-uuid :block/title content}]
                                             (uuid target-id)
                                             {:bottom? true
                                              :sibling? false
                                              :keep-uuid? true}))]
      (swap! created-ids* conj block-uuid)
      (citation content block-uuid))))

(defn- <edit-block!
  "Replaces a block's own text; its children and properties are untouched, which is why
   this is the confirmation the panel makes the user read hardest."
  [{:keys [target-id page-uuid content]}]
  (p/let [_ (ui-outliner-tx/transact!
              {:outliner-op :save-block :ui/page-id page-uuid}
              (outliner-op/save-block! {:block/uuid (uuid target-id)
                                        :block/title content}))]
    (citation content target-id)))

(defn- <delete-block!
  [{:keys [target-id page-uuid target-title]}]
  (p/let [_ (ui-outliner-tx/transact!
              {:outliner-op :delete-blocks :ui/page-id page-uuid}
              (outliner-op/delete-blocks! [(uuid target-id)] {}))]
    (str "已删除块 " (citation target-title target-id))))

(defn- <create-page!
  [{:keys [page-title]}]
  ;; The outliner resolves an existing or recycled page instead of duplicating it, and
  ;; answers with its uuid either way.
  (p/let [[_ page-uuid] (ui-outliner-tx/transact!
                          {:outliner-op :create-page}
                          (outliner-op/create-page! page-title {}))]
    (citation page-title page-uuid)))

(defn- <set-property!
  [{:keys [target-id target-title property-ident property-title value]}]
  (p/let [_ (property-handler/set-block-property! (uuid target-id) property-ident value)]
    (str (citation target-title target-id) " 的属性 " property-title " = " value)))

(defn- <commit!
  "The approved write itself, done here rather than in main because this is where a
   graph write joins the outliner transaction queue and repaints the UI. Resolves with
   the text the model reads back as the tool result."
  [deps {:keys [op] :as request}]
  (cond
    (#{"append-block" "append-child-block"} op) (<append-block! deps request)
    (= "create-page" op) (<create-page! request)
    (= "edit-block" op) (<edit-block! request)
    (= "set-property" op) (<set-property! request)
    (= "delete-block" op) (<delete-block! request)))

(defn- reply!
  "Reports one write decision twice: the result text goes back to the model as the tool
   result, `text` goes into the conversation for the user."
  [{:keys [entries*]} {:keys [confirm-id]} status result text]
  (when (seq text)
    (swap! entries* conj {:role "system" :text text}))
  (ipc/ipc :agent/confirm-reply confirm-id {:status status :result result}))

(defn- write-and-reply!
  [{:keys [entries*] :as deps} request]
  (let [action (action-text request)]
    (-> (<commit! deps request)
        (p/then (fn [result]
                  (reply! deps request "written" result (t :agent.write/success action))))
        (p/catch (fn [^js error]
                   (let [message (.-message error)]
                     (reply! deps request "error" message (t :agent.write/error message))))))))

(defn- refuse-write!
  [deps request]
  (reply! deps request "rejected" "用户拒绝了这次写入，没有改动任何内容"
          (t :agent.write/rejected)))

(defn- ask-write!
  "Shows the confirmation, or answers the run directly when the proposal cannot be
   carried out at all. Either way the run in main stays parked until this replies over
   IPC, and a conversation that already allowed the add-only writes goes straight to
   committing one."
  [{:keys [confirm* auto-approve* created-ids* run-id*] :as deps} request]
  (p/let [request' (<prepare-request request created-ids*)]
    ;; That read takes time, and a cancel or a session switch during it means nobody is
    ;; left waiting on this decision.
    (when (= (:run-id request) @run-id*)
      (cond
        (:error request')
        (reply! deps request' "error"
                (get-in request' [:error :result])
                (get-in request' [:error :text]))

        (and @auto-approve* (direct-ops (:op request')))
        (write-and-reply! deps request')

        :else
        (reset! confirm* request')))))

(defn- persist!
  "Writes the finished turn to the session file. Runs only on `:done`, so a
   failed or cancelled turn never leaves an unanswered user message on disk."
  [{:keys [entries* session-id* title*]}]
  (let [storable (storable-entries @entries*)]
    (when (seq storable)
      (when-not @title*
        (reset! title* (some #(when (= "user" (:role %)) (->title (:text %))) storable)))
      (-> (ipc/ipc :agent/save-session (state/get-current-repo)
                   {:id @session-id*
                    :title @title*
                    :entries storable})
          (p/catch (fn [^js e]
                     (swap! entries* conj {:role "system"
                                           :text (t :agent.panel/save-error (.-message e))})))))))

(defn- rich-entry
  "Ends the streaming phase of an assistant turn: its text is complete, so it is
   rendered as markdown once here instead of on every repaint."
  [entry]
  (if (and (= "assistant" (:role entry)) (seq (:text entry)))
    (assoc entry :rich? true :html (markdown-html (:text entry)))
    entry))

(defn- mark-rich!
  [entries*]
  (swap! entries*
         (fn [entries]
           (if (= "assistant" (:role (last entries)))
             (update-in entries [(dec (count entries))] rich-entry)
             entries)))
  nil)

(defn- handle-event!
  [{:keys [entries* running* run-id* confirm* usage*] :as deps} ^js event]
  ;; Values cross IPC as strings; `bean/->clj` only keywordizes keys.
  (let [{:keys [type run-id delta reasoning ms id name result calls message content
                confirm-id op target-id property-id page-title value attempt
                prompt-tokens completion-tokens]} (bean/->clj event)
        type (keyword type)]
    ;; Events carry the run they belong to. Cancelling reaches the model client, not
    ;; work already dispatched, so a run can still emit after the panel has moved on;
    ;; without this guard that late text lands in whichever session is now open.
    (when (= run-id @run-id*)
      (if (#{:text :reasoning} type)
        (queue-delta! deps type
                      (case type :text delta reasoning)
                      (case type :text 0 ms))
        ;; Anything else paints its own row, so whatever is still in the delta buffer
        ;; has to land first or the rows come out of order.
        (do (flush-deltas! deps)
            (mark-rich! entries*)
            (let [step (fn [state patch]
                         (record-step! entries* (or id name) name state patch))]
              (case type
                ;; The model names a call while its arguments are still streaming.
                :tool-planned (doseq [call calls]
                                (record-step! entries* (or (:id call) (:name call))
                                              (:name call) "planning" {}))

                :tool-call (step "running" {})

                :tool-result (step "ok" {:result result})

                :tool-error (step "error" {:message message})

                :loop-limit (swap! entries* conj {:role "system"
                                                  :text (t :agent.panel/loop-stopped)})

                :turn-limit (swap! entries* conj {:role "system"
                                                  :text (t :agent.panel/rounds-exhausted)})

                :retry (swap! entries* conj {:role "system"
                                             :text (t :agent.panel/retrying attempt message)})

                :usage (reset! usage* {:prompt-tokens prompt-tokens
                                       :completion-tokens completion-tokens})

                ;; The run in main stays parked until the panel answers this back.
                :confirm (do (record-step! entries* (or id name) name "awaiting" {:write? true})
                             (ask-write! deps {:confirm-id confirm-id
                                               :run-id run-id
                                               :op op
                                               :target-id target-id
                                               :property-id property-id
                                               :page-title page-title
                                               :content content
                                               :value value}))

                :cancelled (do (reset! running* false)
                               (reset! run-id* nil)
                               (reset! confirm* nil)
                               (swap! entries* conj {:role "system"
                                                     :text (t :agent.panel/cancelled)}))

                :error (do (reset! running* false)
                           (reset! run-id* nil)
                           (reset! confirm* nil)
                           (swap! entries* conj {:role "system"
                                                 :text (t :agent.panel/error message)}))

                :done (do (reset! running* false)
                          (reset! run-id* nil)
                          (reset! confirm* nil)
                          (when (seq content)
                            (persist! deps)))

                nil)))))))

(defn- new-session-id!
  []
  (str (random-uuid)))

(defn- new-run-id!
  "Identifies one submitted run. The panel owns it, not main: only the panel knows
   which session the events should be painted into."
  []
  (str (random-uuid)))

(defn- apply-session!
  [session {:keys [entries* session-id* title*]}]
  ;; A restored turn is by definition finished, so it renders as markdown.
  (reset! entries* (mapv #(rich-entry (select-keys % [:role :text])) (:entries session)))
  (reset! session-id* (:id session))
  (reset! title* (some-> (:title session) not-empty))
  nil)

(defn- <open-latest!
  "Restores the most recent session, or starts a fresh one when none exists."
  [repo {:keys [session-id* title* entries*] :as deps}]
  (p/let [sessions (ipc/ipc :agent/list-sessions repo)]
    (if-let [latest (first sessions)]
      (p/let [session (ipc/ipc :agent/get-session repo (:id latest))]
        (if session
          (apply-session! session deps)
          (do (reset! entries* [])
              (reset! session-id* (new-session-id!))
              (reset! title* nil))))
      (do (reset! entries* [])
          (reset! session-id* (new-session-id!))
          (reset! title* nil)))
    nil))

(defn- thinking-block
  "Collapsed by default, so the reasoning text never has to stream to look right."
  [{:keys [thinking-ms]} reasoning]
  [:details.agent-thinking
   [:summary (t :agent.panel/thought (js/Math.ceil (/ thinking-ms 1000)))]
   [:div.agent-thinking-body reasoning]])

(defn- step-block
  "One tool call as a collapsible row: the label says what the agent is doing, the
   body is the text the model actually got back. Failed steps open themselves
   because a silent failure is what makes an answer look made up. A write row says
   so instead of the search phrasing, because nothing was searched."
  [{:keys [name state result message write?]}]
  (let [label (if write?
                (case state
                  "ok" (t :agent.write/done-label)
                  "error" (t :agent.write/error-label)
                  (t :agent.write/pending-label))
                (case state
                  "ok" (t :agent.panel/tool-done name)
                  "error" (t :agent.panel/tool-failed name)
                  (t :agent.panel/using-tool name)))
        body (case state "error" message "ok" result nil)]
    (if (string/blank? body)
      [:div.agent-step
       {:class (str "agent-step-" state)}
       [:span.agent-step-label label]]
      [:details.agent-step
       {:class (str "agent-step-" state)
        :open (= "error" state)}
       [:summary.agent-step-label label]
       [:pre.agent-step-body body]])))

(defn- message-body
  "A finished turn is the markdown the panel rendered when it ended, with the graph
   citations inside it clickable; a streaming one is plain text, because markdown of
   a partial sentence flickers between half-parsed shapes."
  [{:keys [rich? html text]}]
  (cond
    (and rich? (seq html))
    [:div.agent-md
     {:dangerouslySetInnerHTML {:__html html}
      :on-click on-citation-click!}]

    (not (string/blank? text))
    text))

(defn- history-popup
  [{:keys [sessions current-id on-switch on-delete on-close]}]
  [:div.cp__agent-history-overlay
   {:on-click on-close}
   [:div.cp__agent-history
    {:on-click (fn [^js e] (.stopPropagation e))}
    (if (empty? sessions)
      [:div.cp__agent-history-empty (t :agent.panel/history-empty)]
      (for [session sessions]
        (let [active? (= (:id session) current-id)]
          [:div.cp__agent-history-item
           {:key (:id session)
            :class (when active? "active")
            :on-click (fn [] (on-switch (:id session)))}
           [:span.cp__agent-history-title
            (let [title (:title session)]
              (if (seq title) title (t :agent.panel/new-chat)))]
           (shui/button-ghost-icon :trash
                                   {:class "cp__agent-history-delete"
                                    :aria-label (t :ui/delete)
                                    :on-click (fn [^js e]
                                                (.stopPropagation e)
                                                (on-delete (:id session)))})])))]])

(defn- confirm-popup
  "The write decision, in the panel rather than as a native dialog. It names the write
   and its target, shows the text that would replace what is there for the ops that
   touch existing content, and shows the text to be added. Only what is on this card is
   what the commit sends. `本会话允许` appears for the add-only writes and never for the
   others, so allowing one can never turn a later edit or deletion into a silent yes."
  [{:keys [request on-reject on-approve on-approve-session!]}]
  (let [{:keys [op content current-text property-title value]} request]
    [:div.cp__agent-confirm-overlay
     {:on-click on-reject}
     [:div.cp__agent-confirm
      {:on-click (fn [^js e] (.stopPropagation e))}
      [:div.cp__agent-confirm-title (t :agent.write/confirm-title)]
      [:div.cp__agent-confirm-action (action-text request)]
      (when (seq current-text)
        [:<>
         [:div.cp__agent-confirm-label (t :agent.write/current-content-label)]
         [:div.cp__agent-confirm-current current-text]])
      (when (seq content)
        [:<>
         [:div.cp__agent-confirm-label (t :agent.write/new-content-label)]
         [:div.cp__agent-confirm-new content]])
      (when (= "set-property" op)
        [:div.cp__agent-confirm-property
         (t :agent.write/property-label (short-title property-title) value)])
      [:div.cp__agent-confirm-actions
       [:button.button.cp__agent-confirm-reject
        {:on-click on-reject}
        (t :agent.write/reject)]
       (when (direct-ops op)
         [:button.button.cp__agent-confirm-session
          {:on-click on-approve-session!}
          (t :agent.write/allow-session)])
       [:button.button.cp__agent-confirm-approve
        {:on-click on-approve}
        (t :agent.write/approve)]]]]))

(hsx/defc panel
  []
  (let [repo (rfx/use-sub [:git/current-repo])
        entries* (hooks/use-memo #(atom []) [])
        input* (hooks/use-memo #(atom "") [])
        running* (hooks/use-memo #(atom false) [])
        agent-state* (hooks/use-memo #(atom nil) [])
        session-id* (hooks/use-memo #(atom (new-session-id!)) [])
        title* (hooks/use-memo #(atom nil) [])
        sessions* (hooks/use-memo #(atom nil) [])
        run-id* (hooks/use-memo #(atom nil) [])
        deltas* (hooks/use-memo #(atom nil) [])
        timer* (hooks/use-memo #(atom nil) [])
        confirm* (hooks/use-memo #(atom nil) [])
        auto-approve* (hooks/use-memo #(atom false) [])
        created-ids* (hooks/use-memo #(atom #{}) [])
        usage* (hooks/use-memo #(atom nil) [])
        following* (hooks/use-memo #(atom true) [])
        *log-el (hooks/use-ref nil)
        [entries] (hooks/use-atom entries*)
        [input] (hooks/use-atom input*)
        [running?] (hooks/use-atom running*)
        [agent-state] (hooks/use-atom agent-state*)
        [sessions] (hooks/use-atom sessions*)
        [confirm] (hooks/use-atom confirm*)
        [usage] (hooks/use-atom usage*)
        [following] (hooks/use-atom following*)
        deps {:entries* entries* :running* running* :run-id* run-id*
              :session-id* session-id* :title* title*
              :deltas* deltas* :timer* timer* :confirm* confirm*
              :auto-approve* auto-approve* :created-ids* created-ids*
              :usage* usage*}
        scroll-to-bottom! (fn []
                            (when-let [^js el (hooks/deref *log-el)]
                              (set! (.-scrollTop el) (.-scrollHeight el))))
        ;; Anything further than the threshold from the bottom is the user having
        ;; scrolled away on purpose, so the next delta must not yank them back.
        on-scroll! (fn [^js e]
                     (let [^js el (.-target e)]
                       (reset! following*
                               (<= (- (.-scrollHeight el)
                                      (.-scrollTop el)
                                      (.-clientHeight el))
                                   bottom-threshold-px))))
        start-session! (fn []
                         (reset! entries* [])
                         (reset! deltas* nil)
                         (reset! confirm* nil)
                         (reset! usage* nil)
                         (reset! auto-approve* false)
                         ;; A block this conversation wrote is the only thing its
                         ;; delete_block may take, so the list starts over with it.
                         (reset! created-ids* #{})
                         (reset! following* true))
        cancel-run! (fn []
                      ;; Drop the run's identity first: its remaining events are
                      ;; worthless now, and abort only reaches the model client, so
                      ;; the run itself can outlive this call.
                      (when @run-id*
                        (reset! run-id* nil)
                        (reset! running* false)
                        (reset! confirm* nil)
                        (ipc/ipc :agent/cancel)))
        refresh! (fn []
                   (p/let [state' (ipc/ipc :agent/state)]
                     (reset! agent-state* state')
                     nil))
        new-chat! (fn []
                    (cancel-run!)
                    (start-session!)
                    (reset! session-id* (new-session-id!))
                    (reset! title* nil)
                    (reset! sessions* nil))
        open-history! (fn []
                        (if sessions
                          (reset! sessions* nil)
                          (p/let [list (ipc/ipc :agent/list-sessions repo)]
                            (reset! sessions* (or list []))
                            nil)))
        switch-session! (fn [id]
                          (cancel-run!)
                          (start-session!)
                          (when (not= id @session-id*)
                            (p/let [session (ipc/ipc :agent/get-session repo id)]
                              (when session
                                (apply-session! session deps))))
                          (reset! sessions* nil))
        delete-session! (fn [id]
                          (cancel-run!)
                          (p/let [_ (ipc/ipc :agent/remove-session repo id)
                                  _ (when (= id @session-id*)
                                      (<open-latest! repo deps))
                                  list (ipc/ipc :agent/list-sessions repo)]
                            (reset! sessions* (or list []))
                            nil))
        stop-run! (fn []
                    (cancel-run!)
                    ;; A stop the user asked for is known here already, so say so
                    ;; instead of waiting for an event that the guard now discards.
                    (swap! entries* conj {:role "system" :text (t :agent.panel/cancelled)}))
        approve-write! (fn []
                         (when-let [request @confirm*]
                           (reset! confirm* nil)
                           (write-and-reply! deps request)))
        approve-session! (fn []
                           (reset! auto-approve* true)
                           (swap! entries* conj {:role "system"
                                                  :text (t :agent.write/session-allowed-tip)})
                           (approve-write!))
        reject-write! (fn []
                        (when-let [request @confirm*]
                          (reset! confirm* nil)
                          (refuse-write! deps request)))
        submit! (fn []
                  (let [text (string/trim @input*)]
                    (when (and (seq text) (not @running*) repo)
                      (let [history (wire-history @entries*)
                            run-id (new-run-id!)
                            ;; Main cannot see the screen, so the panel says what the
                            ;; user is looking at: it is the cheapest way for the agent
                            ;; to answer about "this page" without searching blindly.
                            page-title (some-> (state/get-current-page)
                                               not-empty
                                               js/decodeURIComponent)
                            selection (editor-handler/get-selected-text)]
                        (reset! input* "")
                        (reset! running* true)
                        (reset! run-id* run-id)
                        (reset! following* true)
                        (swap! entries* conj {:role "user" :text text})
                        (-> (ipc/ipc :agent/run repo text history run-id page-title selection)
                            (p/catch (fn [^js e]
                                       (when (= run-id @run-id*)
                                         (reset! running* false)
                                         (reset! run-id* nil)
                                         (swap! entries* conj {:role "system"
                                                               :text (t :agent.panel/error (.-message e))})))))))))]
    (hooks/use-effect!
     (fn []
       (when (and repo (util/electron?))
         (cancel-run!)
         (refresh!)
         (<open-latest! repo deps))
       nil)
     [repo])

    (hooks/use-effect!
     (fn []
       (let [on-event (fn [^js data] (handle-event! deps data))
             ^js apis js/window.apis]
         (.on apis "agent-event" on-event)
         #(do (.off apis "agent-event" on-event)
              ;; Whatever the delta buffer still holds is not coming back.
              (when-let [^js timer @timer*]
                (js/clearTimeout timer)))))
     [])

    (hooks/use-effect!
     (fn []
       (when @following*
         (scroll-to-bottom!))
       nil)
     [entries running? following])

    [:div.cp__agent-panel.relative.flex.flex-col.h-full
     [:div.cp__agent-panel-topbar.flex.items-center.justify-between.gap-2
      [:div.font-bold (t :agent.panel/title)]
      [:div.flex.items-center.gap-1
       (ui/tooltip
        (shui/button-ghost-icon :message-plus
                                {:class "agent-panel-new-chat"
                                 :on-click new-chat!})
        (t :agent.panel/new-chat))
       (ui/tooltip
        (shui/button-ghost-icon :history
                                {:class "agent-panel-history"
                                 :on-click open-history!})
        (t :agent.panel/history))
       (ui/button (t :agent.panel/settings)
                  :small? true
                  :on-click #(state/open-settings! :ai))
       (shui/button-ghost-icon :x {:class "agent-panel-close"
                                   :aria-label (t :ui/close)
                                   :on-click ui-handler/hide-agent-panel!})]]

     (when sessions
       (history-popup {:sessions sessions
                       :current-id @session-id*
                       :on-switch switch-session!
                       :on-delete delete-session!
                       :on-close #(reset! sessions* nil)}))

     (when confirm
       (confirm-popup {:request confirm
                       :on-reject reject-write!
                       :on-approve approve-write!
                       :on-approve-session! approve-session!}))

     (when (and agent-state (not (:enabled? agent-state)))
       [:div.cp__agent-panel-hint.px-3 (t :agent.panel/disabled)])

     (when (and agent-state (:enabled? agent-state) (not (:configured? agent-state)))
       [:div.cp__agent-panel-hint.px-3 (t :agent.panel/not-configured)])

     [:div.cp__agent-panel-log.flex-1.overflow-y-auto.flex.flex-col.gap-2.scrollbar-spacing.px-3
      {:ref *log-el
       :on-scroll on-scroll!}
      (if (empty? entries)
        [:div.cp__agent-panel-empty (t :agent.panel/empty)]
        (for [[idx entry] (map-indexed vector entries)]
          ^{:key idx}
          [:div {:class (str "agent-message agent-message-" (:role entry))}
           (when (= "tool" (:role entry))
             (step-block entry))
           (when-let [reasoning (:reasoning entry)]
             (thinking-block entry reasoning))
           (message-body entry)]))
      (when running?
        [:div.agent-message.agent-message-system (t :agent.panel/thinking)])]

     (when (and (not following) (seq entries))
       [:button.button.cp__agent-jump-bottom
        {:on-click (fn []
                     (reset! following* true)
                     (scroll-to-bottom!))}
        (t :agent.panel/back-to-bottom)])

     (when (seq usage)
       [:div.agent-usage
        (t :agent.panel/usage (:prompt-tokens usage) (:completion-tokens usage))])

     [:div.cp__agent-panel-input.flex.gap-1
      [:textarea.flex-1.form-input
       {:value input
        :placeholder (t :agent.panel/placeholder)
        :on-change (fn [^js e] (reset! input* (.. e -target -value)))
        :on-key-down (fn [^js e]
                       ;; Enter also commits an IME candidate, which is the common case
                       ;; on a Chinese keyboard; sending then would post half a sentence.
                       (when (and (= "Enter" (.-key e))
                                  (not (.-shiftKey e))
                                  (not (util/native-event-is-composing? e)))
                         (.preventDefault e)
                         (submit!)))}]
      (if running?
        [:button.button {:on-click stop-run!}
         (t :agent.panel/cancel)]
        [:button.button {:on-click submit!}
         (t :agent.panel/send)])]]))

(hsx/defc resizer
  []
  (let [*el-ref (hooks/use-ref nil)
        ^js el-doc js/document.documentElement
        min-px 260
        main-min-px 320
        set-width! (fn [target-px]
                     (ui-handler/persist-agent-panel-width!
                      (str (js/Math.round target-px) "px")))]
    (hooks/use-effect!
     (fn []
       (when-let [el (and (fn? js/window.interact) (hooks/deref *el-ref))]
         (-> (js/interact el)
             (.draggable
              #js {:listeners
                   #js {:move (fn [^js/MouseEvent e]
                                (let [viewport-px (.-clientWidth el-doc)
                                      width-px (- viewport-px (.-pageX e))
                                      max-px (- viewport-px main-min-px)]
                                  (set-width! (max min-px (min max-px width-px)))))}})
             (.styleCursor false)
             (.on "dragstart" #(.. el-doc -classList (add "is-resizing-buf")))
             (.on "dragend" #(.. el-doc -classList (remove "is-resizing-buf")))))
       #())
     [])
    [:.resizer {:ref *el-ref
                :role "separator"
                :aria-orientation "vertical"
                :aria-label (t :agent.panel/resize-handle)}]))

(hsx/defc toggle
  []
  (when (and (util/electron?) (not (util/sm-breakpoint?)))
    (ui/tooltip
     (shui/button-ghost-icon :wand
                             {:class "toggle-agent-panel"
                              :on-click ui-handler/toggle-agent-panel!})
     (t :agent.panel/toggle))))

(hsx/defc sidebar-component
  []
  (let [open? (rfx/use-sub [:ui/agent-panel-open?])
        width (rfx/use-sub [:ui/agent-panel-width])]
    [:div#agent-panel.cp__agent-sidebar.h-screen
     {:class (if open? "open" "closed")
      :style {:width width}}
     (resizer)
     (when open?
       (panel))]))

(def sidebar
  (let [memo-class (react/memo
                    (fn [_props]
                      (sidebar-component)))]
    (fn []
      (react/createElement memo-class #js {}))))
