(ns electron.agent.session
  "Out-of-graph session storage for the agent panel.

   Each graph gets a directory under `userData/agent-sessions/<repo-key>/`:
   `index.json` maps session id to list metadata, `<id>.json` holds one
   session's entries. Conversations never enter the graph itself."
  (:require ["electron" :refer [^js app]]
            ["fs-extra" :as ^js fs]
            ["path" :as ^js node-path]
            [electron.logger :as logger]))

(defonce ^:private sessions-root
  (.join node-path (.getPath app "userData") "agent-sessions"))

(defn- repo-key
  [repo]
  (.replace (str repo) #"[^\w.-]" "_"))

(defn- repo-dir
  [repo]
  (.join node-path sessions-root (repo-key repo)))

(defn- index-path
  [repo]
  (.join node-path (repo-dir repo) "index.json"))

(defn- session-path
  [repo id]
  (let [id (str id)]
    (when-not (re-matches #"[\w-]+" id)
      (throw (ex-info "Invalid agent session id" {:id id})))
    (.join node-path (repo-dir repo) (str id ".json"))))

(defn- read-json
  [path]
  (try
    (when (.existsSync fs path)
      (js->clj (js/JSON.parse (.toString (.readFileSync fs path)))
               :keywordize-keys true))
    (catch :default e
      (logger/error "agent session read failed" {:path path :error (str e)})
      nil)))

(defn- write-json!
  "Writes through a sibling temp file and renames, so an interrupted save leaves the
   previous version intact instead of a half-written file that the next read of
   `index.json` would fail on."
  [path value]
  (let [tmp (str path "." (js/Date.now) "-" (js/Math.random) ".tmp")]
    (.ensureDirSync fs (.dirname node-path path))
    (.writeFileSync fs tmp (js/JSON.stringify (clj->js value) nil 2))
    (.renameSync fs tmp path)))

(defn- read-index
  [repo]
  (or (:sessions (read-json (index-path repo))) {}))

(defn- write-index!
  [repo index]
  (write-json! (index-path repo) {:sessions index}))

(defn list-sessions
  "Index entries for the renderer's history list, most recently updated first."
  [repo]
  (->> (vals (read-index repo))
       (sort-by :updated-at >)
       (vec)))

(defn get-session
  [repo id]
  (read-json (session-path repo id)))

(defn save-session!
  "Creates or overwrites one session and upserts its index entry."
  [repo {:keys [id title entries created-at] :as session}]
  (when-not (and id (vector? entries))
    (throw (ex-info "Invalid agent session payload" {:id id})))
  (let [path (session-path repo id)
        now (.now js/Date)
        created-at (or (:created-at (read-json path)) created-at now)
        ;; A blank title stays blank; the renderer falls back to its own
        ;; localized "New chat" label when listing sessions.
        session (-> session
                    (assoc :id id
                           :title (or title "")
                           :entries entries
                           :created-at created-at
                           :updated-at now)
                    (select-keys [:id :title :entries :created-at :updated-at]))]
    (write-json! (session-path repo id) session)
    (write-index! repo (assoc (read-index repo)
                              id (dissoc session :entries)))
    (dissoc session :entries)))

(defn remove-session!
  [repo id]
  (let [path (session-path repo id)]
    (when (.existsSync fs path)
      (.removeSync fs path)))
  (write-index! repo (dissoc (read-index repo) id))
  nil)
