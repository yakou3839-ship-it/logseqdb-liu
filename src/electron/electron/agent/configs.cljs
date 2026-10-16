(ns electron.agent.configs
  "AI provider presets and the persisted agent config of the desktop app."
  (:require [clojure.string :as string]
            [electron.configs :as cfgs]))

(def ^:private providers-key :ai/providers)
(def ^:private active-provider-key :ai/active-provider)
(def ^:private active-model-key :ai/active-model)
(def ^:private enabled-key :feature/enable-agent?)

(def protocol-openai
  "Only protocol implemented so far; kept as a field so a second protocol does
   not require reshaping stored providers."
  :openai)

(def protocols
  ["openai"])

(defn clamp-timeout
  "Matches SiYuan's kernel rule: integer seconds in 1-600, default 120."
  [timeout]
  (if (number? timeout)
    (max 1 (min 600 (int timeout)))
    120))

(def provider-presets
  "Ported from SiYuan's aiProviderPresets.ts, minus Anthropic: its endpoint only
   speaks the Messages protocol, which chat/completions callers cannot use."
  [{:id "openai" :display-name "OpenAI" :base-url "https://api.openai.com/v1" :category :official}
   {:id "deepseek" :display-name "DeepSeek" :base-url "https://api.deepseek.com" :category :official}
   {:id "moonshot" :display-name "Moonshot AI" :base-url "https://api.moonshot.cn/v1" :category :official}
   {:id "minimax" :display-name "MiniMax" :base-url "https://api.minimax.io/v1" :category :official}
   {:id "minimax-cn" :display-name "MiniMax" :base-url "https://api.minimax.cn/v1" :category :official}
   {:id "aliyun" :display-name "Alibaba Model Studio" :base-url "https://dashscope.aliyuncs.com/compatible-mode/v1" :category :official}
   {:id "aliyun-intl" :display-name "Alibaba Model Studio" :base-url "https://dashscope-intl.aliyuncs.com/compatible-mode/v1" :category :official}
   {:id "volcengine" :display-name "Volcengine Ark" :base-url "https://ark.cn-beijing.volces.com/api/v3" :category :official}
   {:id "zhipu" :display-name "Zhipu AI" :base-url "https://open.bigmodel.cn/api/paas/v4" :category :official}
   {:id "gemini" :display-name "Gemini" :base-url "https://generativelanguage.googleapis.com/v1beta/openai" :category :official}
   {:id "mistral" :display-name "Mistral AI" :base-url "https://api.mistral.ai/v1" :category :official}
   {:id "siliconflow" :display-name "SiliconFlow" :base-url "https://api.siliconflow.cn/v1" :category :aggregator}
   {:id "openrouter" :display-name "OpenRouter" :base-url "https://openrouter.ai/api/v1" :category :aggregator}
   {:id "groq" :display-name "Groq" :base-url "https://api.groq.com/openai/v1" :category :aggregator}
   {:id "ollama" :display-name "Ollama" :base-url "http://localhost:11434/v1" :category :local}
   {:id "lmstudio" :display-name "LM Studio" :base-url "http://localhost:1234/v1" :category :local}
   {:id "custom" :display-name "Custom" :base-url "" :category :custom}])

(defn preset
  [preset-id]
  (first (filter #(= (:id %) preset-id) provider-presets)))

(defn- preset->provider
  [preset-id]
  (when-let [entry (preset preset-id)]
    (assoc entry
           :protocol protocol-openai
           :enabled true
           :request-timeout 120
           :active-model nil
           :models [])))

(defn get-providers
  []
  (cfgs/get-item providers-key))

(defn find-provider
  [provider-id]
  (first (filter #(= (:id %) provider-id) (get-providers))))

(defn normalize-provider
  "Coerces a wire-format provider map into the storage shape. Transit keeps
   keyword keys across IPC, so the renderer sends and receives this same shape.
   A blank :api-key or :base-url keeps the stored value, so re-saving an edited
   provider never wipes its secret or endpoint. Fields the wire omits entirely
   (the enable toggle sends only :id and :enabled) stay unset here so that
   `upsert-provider!` merges the stored value instead of a fresh default."
  [{:keys [id display-name base-url api-key protocol request-timeout
           enabled models] :as wire}]
  (when (string/blank? id)
    (throw (ex-info "Provider requires an id" {:provider wire})))
  (let [existing (find-provider id)
        models' (when (vector? models)
                  (mapv (fn [{:keys [id name display-name enabled]}]
                          (let [id' (if (string/blank? id) name id)
                                name' (if (string/blank? name) id' name)]
                            {:id id'
                             :name name'
                             :display-name (when-not (string/blank? display-name) display-name)
                             :enabled (if (false? enabled) false true)}))
                        models))]
    (cond-> {:id id
             :enabled (if (false? enabled) false true)}
      (some? protocol) (assoc :protocol (keyword protocol))
      (some? request-timeout) (assoc :request-timeout (clamp-timeout request-timeout))
      (not (string/blank? display-name)) (assoc :display-name display-name)
      (or (string/blank? base-url) (not= base-url (:base-url existing)))
      (assoc :base-url (if (string/blank? base-url)
                         (:base-url existing)
                         base-url))
      (not (string/blank? api-key)) (assoc :api-key api-key)
      (some? models') (assoc :models models'))))

(defn upsert-provider!
  "Stores `provider` by :id, appending when it is new. Merged over the stored
   entry so fields the wire form omits (:active-model) survive."
  [provider]
  (let [{:keys [id] :as provider} provider
        providers (vec (get-providers))
        existing (first (filter #(= (:id %) id) providers))
        provider' (merge existing provider)
        providers' (if existing
                     (mapv #(if (= (:id %) id) provider' %) providers)
                     (conj providers provider'))]
    (cfgs/set-item! providers-key providers')
    provider'))

(defn install-preset!
  [preset-id]
  (if-let [provider (preset->provider preset-id)]
    (upsert-provider! provider)
    (throw (ex-info "Unknown AI provider preset" {:preset-id preset-id}))))

(defn remove-provider!
  [provider-id]
  (cfgs/set-item! providers-key
                  (vec (remove #(= (:id %) provider-id) (get-providers)))))

(defn set-active!
  "Records the active provider/model globally and on the provider itself, so
   each card remembers its own picked model."
  [provider-id model-name]
  (cfgs/set-item! active-provider-key provider-id)
  (cfgs/set-item! active-model-key model-name)
  (when-let [provider (find-provider provider-id)]
    (upsert-provider! (assoc provider :active-model model-name))))

(defn active-provider-id
  []
  (cfgs/get-item active-provider-key))

(defn active-provider
  "The configured provider, or nil when nothing is configured yet."
  []
  (when-let [provider-id (active-provider-id)]
    (find-provider provider-id)))

(defn active-model-name
  []
  (or (cfgs/get-item active-model-key)
      (:active-model (active-provider))))

(defn configured?
  "An active model is only usable when its provider carries a base url and key."
  []
  (boolean (when-let [provider (active-provider)]
             (and (seq (:base-url provider))
                  (seq (:api-key provider))
                  (seq (active-model-name))))))

(defn enabled?
  []
  (true? (cfgs/get-item enabled-key)))

(defn set-enabled!
  [enabled]
  (cfgs/set-item! enabled-key (boolean enabled)))
