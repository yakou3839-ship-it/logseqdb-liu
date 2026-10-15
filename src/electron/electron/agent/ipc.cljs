(ns electron.agent.ipc
  "Main-process side of the agent IPC surface.

   Plain functions only; `electron.handler` owns the defmethods that call these
   (repo convention). Each window owns at most one run at a time, and that run's
   AbortSignal is the single source of truth for \"the user stopped this\": the loop
   polls it, the model client aborts its request through it, and this namespace
   reads it to decide which terminal event to emit. Cancelling also wakes a write the
   panel has been shown but not yet answered.

   A write the model proposes comes back here as a request for approval: the event is
   emitted on `event-channel` and the run parks until `confirm-reply!` settles. An
   unanswered approval is never a silent yes — it expires, and it dies with the run."
  (:require [clojure.string :as string]
            [electron.agent.configs :as configs]
            [electron.agent.core :as agent]
            [electron.agent.session :as session]
            [electron.logger :as logger]
            [electron.utils :refer [send-to-renderer]]
            [promesa.core :as p]))

(def event-channel :agent-event)

(defonce ^:private *runs (atom {})) ;; window-id -> {:controller .. :run-id ..}
(defonce ^:private *confirms (atom {})) ;; window-id -> {confirm-id {:resolve :reject}}

;; A confirmation the panel never answers. Long enough that a user away from the
;; keyboard can come back and decide, short enough that a wedged renderer cannot
;; hold a run open.
(def ^:private confirm-timeout-ms (* 10 60 1000))

(defn- without-api-key
  [provider]
  (dissoc provider :api-key))

(defn- provider->wire
  "Provider shape sent to the renderer: keys never leave main, and a key's
   presence is all the UI needs to show."
  [provider]
  (-> (without-api-key provider)
      (assoc :has-api-key? (boolean (seq (:api-key provider))))))

(defn state
  "Agent config for the renderer; API keys never leave the main process."
  []
  {:enabled? (configs/enabled?)
   :configured? (configs/configured?)
   :providers (mapv provider->wire (configs/get-providers))
   :presets configs/provider-presets
   :active-provider (configs/active-provider-id)
   :active-model (configs/active-model-name)})

(defn install-preset!
  [preset-id]
  (without-api-key (configs/install-preset! preset-id)))

(defn upsert-provider!
  [wire-provider]
  (without-api-key (configs/upsert-provider!
                    (configs/normalize-provider wire-provider))))

(defn remove-provider!
  [provider-id]
  (configs/remove-provider! provider-id)
  (state))

(defn set-active!
  [provider-id model-name]
  (when (string/blank? model-name)
    (throw (ex-info "An active model is required" {:provider-id provider-id})))
  (configs/set-active! provider-id model-name)
  (state))

(defn set-enabled!
  [enabled]
  (configs/set-enabled! enabled)
  (state))

(defn- reject-confirms!
  "Wakes every confirmation still waiting on `window`, because the run that asked for
   it is over. The panel's own modal is dismissed by the `:cancelled` event."
  [window-id error]
  (doseq [[_ {:keys [reject]}] (get @*confirms window-id)]
    (reject error)))

(defn confirm-reply!
  "Settles the confirmation `confirm-id` with the panel's decision, which is the text
   the model reads back as the tool result."
  [window-id confirm-id reply]
  (if-let [{:keys [resolve]} (get-in @*confirms [window-id confirm-id])]
    (do (resolve reply)
        true)
    (logger/warn "agent confirm reply for an unknown request"
                 {:window-id window-id :confirm-id confirm-id})))

(defn- <confirm!
  "Asks the panel to approve one write and waits for its answer."
  [window-id emit request]
  (p/create
   (fn [resolve reject]
     (let [confirm-id (str (random-uuid))
           timer* (atom nil)
           settle! (fn [settle]
                     (when-let [^js timer @timer*]
                       (js/clearTimeout timer))
                     (swap! *confirms update-in [window-id] dissoc confirm-id)
                     (settle))]
       (swap! *confirms assoc-in [window-id confirm-id]
              {:resolve #(settle! (resolve %))
               :reject #(settle! (reject %))})
       (reset! timer* (js/setTimeout
                       #(settle! (resolve {:status "expired"
                                           :result "确认已超时，写入未执行"}))
                       confirm-timeout-ms))
       (emit (assoc request :type :confirm :confirm-id confirm-id))))))

(defn cancel!
  "Aborts the running agent run of `window`, if it has one."
  [window-id]
  (when-let [{:keys [controller]} (get @*runs window-id)]
    (.abort ^js controller)
    (reject-confirms! window-id (ex-info "Agent run cancelled" {:cancelled true}))
    true))

(defn list-sessions
  [repo]
  (session/list-sessions repo))

(defn get-session
  [repo id]
  (session/get-session repo id))

(defn save-session!
  [repo wire-session]
  (session/save-session! repo wire-session))

(defn remove-session!
  [repo id]
  (session/remove-session! repo id))

(defn start-run!
  "Starts a run for `window`; streams events to that renderer on `event-channel`
   and resolves with the final assistant text.

   `run-id` is the renderer's, because only the panel knows which session is open:
   every event is tagged with it so a run that outlives a session switch can be
   ignored instead of appending its answer to the newly opened conversation.

   `context` is what the user is looking at when they asked — the current page and
   any selection — which the loop folds into the system prompt.

   A run that fails is reported once, as an event, with the detail logged here.
   The promise never rejects for it, so the panel cannot end up showing the same
   failure twice."
  [^js window repo input history run-id context]
  (when (string/blank? run-id)
    (throw (ex-info "An agent run needs the id its events are tagged with"
                    {:repo repo :window-id (.-id window)})))
  (let [window-id (.-id window)
        _ (cancel! window-id)
        controller (js/AbortController.)
        signal (.-signal controller)
        emit (fn [event]
               (when (= run-id (get-in @*runs [window-id :run-id]))
                 (send-to-renderer window event-channel (assoc event :run-id run-id))))]
    (swap! *runs assoc window-id {:controller controller :run-id run-id})
    (-> (p/let [content (agent/start! repo window-id input history
                                      {:on-event emit
                                       :on-confirm #(<confirm! window-id emit %)
                                       :abort-signal signal
                                       :context context})]
            content)
        (p/catch (fn [error]
                   (if (.-aborted ^js signal)
                     (emit {:type :cancelled})
                     (let [{:keys [status body]} (ex-data error)
                           message (cond-> (str (or (.-message error) error))
                                     status (str " (HTTP " status ")")
                                     (seq body) (str ": " body))]
                       (logger/error "agent run failed"
                                     {:repo repo
                                      :window-id window-id
                                      :run-id run-id
                                      :status status
                                      :response body
                                      :error (str error)})
                       (emit {:type :error :message message})))))
        (p/finally (fn [_]
                     ;; Free only this run's slot: a newer run in the same window has
                     ;; taken over, and dropping its entry would orphan its controller
                     ;; and leave the stop button with nothing to abort.
                     (swap! *runs #(if (= run-id (get-in % [window-id :run-id]))
                                     (dissoc % window-id)
                                     %)))))))
