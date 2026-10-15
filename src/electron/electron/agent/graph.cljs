(ns electron.agent.graph
  "Graph reads for the agent, from the Electron main process.

   Uses the same db-worker-node runtime path the desktop app already uses for
   db backups, so no new worker-side API is introduced here. Nothing in here
   writes: a graph write has to go through the renderer's outliner transaction
   queue, which is why `electron.agent.tools` hands approved writes to the panel."
  (:require [electron.db-worker :as db-worker]
            [logseq.cli.transport :as cli-transport]
            [logseq.common.config :as common-config]
            [promesa.core :as p]))

;; The worker keys its graph connections by the canonical repo name, so callers
;; may pass either "Demo" or "logseq_db_Demo".
(defn- canonical-repo
  [repo]
  (common-config/canonicalize-db-version-repo repo))

;; Concurrent ensure-runtime! calls for the same repo race on the worker's
;; admission registration, so in-flight acquisitions are shared.
(defonce ^:private *runtime-promises (atom {}))

(defn- <runtime!
  [repo window-id]
  (let [key [repo window-id]]
    (or (get @*runtime-promises key)
        (let [pending (p/create
                       (fn [resolve reject]
                         (-> (db-worker/ensure-runtime! repo window-id)
                             (p/then resolve)
                             (p/catch reject))))]
          (swap! *runtime-promises assoc key pending)
          (-> pending
              (p/then (fn [runtime]
                        (swap! *runtime-promises dissoc key)
                        runtime))
              (p/catch (fn [error]
                         (swap! *runtime-promises dissoc key)
                         (p/rejected error))))
          pending))))

(defn <invoke
  "Runs thread-api `method` against `repo`, whose first arg is filled in.
   Requires the graph to already be open in `window-id`'s renderer, which is what
   registers the worker's db connection."
  [repo window-id method args]
  (let [repo' (canonical-repo repo)]
    (p/let [runtime (<runtime! repo' window-id)]
      (cli-transport/invoke runtime method (into [repo'] args)))))

(defn list-pages
  [repo window-id options]
  (<invoke repo window-id :thread-api/api-list-pages [options]))

(defn search-blocks
  [repo window-id q options]
  (<invoke repo window-id :thread-api/search-blocks [q options]))

(defn get-page-data
  [repo window-id page-title]
  (<invoke repo window-id :thread-api/api-get-page-data [page-title]))

(defn list-tags
  [repo window-id options]
  (<invoke repo window-id :thread-api/api-list-tags [options]))

(defn list-properties
  [repo window-id options]
  (<invoke repo window-id :thread-api/api-list-properties [options]))
