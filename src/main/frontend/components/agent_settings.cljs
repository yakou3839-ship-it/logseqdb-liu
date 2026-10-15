(ns frontend.components.agent-settings
  "AI provider configuration inside the app Settings modal's AI tab.
   Structurally ported from SiYuan's AI settings: a preset catalog grouped by
   category, and per-provider cards whose inline editor covers name, endpoint,
   protocol, timeout, API key, and the model list. Keys are typed into main
   and never read back."
  (:require [clojure.string :as string]
            [electron.ipc :as ipc]
            [frontend.context.i18n :refer [t]]
            [frontend.ui :as ui]
            [frontend.util :as util]
            [io.factorhouse.hsx.core :as hsx]
            [logseq.shui.hooks :as hooks]
            [promesa.core :as p]))

(defn- category-title
  [category]
  (case category
    :official (t :agent.settings/category-official)
    :aggregator (t :agent.settings/category-aggregator)
    :local (t :agent.settings/category-local)
    :custom (t :agent.settings/category-custom)))

(defn- draft-defaults
  "Seed an editor draft from a stored provider, so untouched fields round-trip
   instead of being dropped on save."
  [provider]
  {:display-name (:display-name provider)
   :base-url (:base-url provider)
   :protocol "openai"
   :request-timeout (or (:request-timeout provider) 120)
   :models (or (:models provider) [])})

(defn- group-presets
  [presets]
  (remove #(empty? (second %))
          (for [[category items] (group-by :category presets)]
            [category (sort-by :display-name items)])))

(defn- model-label
  [{:keys [display-name name]}]
  (or (not-empty display-name) name))

(defn- remove-nth
  [v idx]
  (into [] (concat (take idx v) (drop (inc idx) v))))

(hsx/defc provider-editor
  "Inline editor for one provider. `base` is the stored wire map; the draft lives
   in the parent's subscribed atom, so `get-field` reads render-current values
   and `set-field` re-renders. Submitting merges base + draft + rebuilt models
   over IPC; a blank key keeps the stored secret."
  [{:keys [base draft key-visible* key-visible? get-field set-field
           on-cancel on-saved]}]
  (let [saving* (hooks/use-memo #(atom false) [])
        [saving?] (hooks/use-atom saving*)
        models' (or (get-field :models) [])
        timeout-str (str (or (get-field :request-timeout) 120))
        model-name (fn [m] (or (:name m) ""))
        can-save? (and (not saving?)
                       (not (string/blank? (or (get-field :base-url) "")))
                       (seq (remove string/blank? (mapv model-name models'))))
        save! (fn []
                (when can-save?
                  (reset! saving* true)
                  (let [models-out (mapv (fn [{:keys [display-name enabled] :as m}]
                                           (let [nm (string/trim (model-name m))]
                                             (cond-> {:id nm :name nm
                                                      :enabled (not (false? enabled))}
                                               (not (string/blank? (str display-name)))
                                               (assoc :display-name display-name))))
                                         (remove #(string/blank? (model-name %)) models'))
                        provider (merge base
                                        (dissoc draft :models)
                                        {:models models-out})]
                    (p/let [_ (ipc/ipc :agent/upsert-provider provider)
                            _ (on-saved)]
                      (reset! saving* false)))))]
    [:div.cp__agent-provider-editor.flex.flex-col.gap-3.p-3.rounded-md.bordered
     [:div.flex.items-center.justify-between
      [:div.text-sm.font-medium.opacity-70 (t :agent.settings/edit-provider)]
      (ui/button (if key-visible? (t :agent.settings/hide-key) (t :agent.settings/show-key))
                 :small? true :class "p-1"
                 :on-click #(swap! key-visible* not))]

     [:div.flex.gap-3
      [:label.flex.flex-col.gap-1.flex-1
       [:span.text-sm.font-medium.opacity-70 (t :agent.settings/provider-name)]
       [:input.form-input
        {:value (or (get-field :display-name) "")
         :on-change #(set-field :display-name (.. % -target -value))}]]
      [:label.flex.flex-col.gap-1.w-28
       [:span.text-sm.font-medium.opacity-70 (t :agent.settings/timeout)]
       [:input.form-input
        {:type "number" :min 1 :max 600
         :title (t :agent.settings/timeout-hint)
         :value timeout-str
         :on-change #(set-field :request-timeout
                                  (let [v (js/parseInt (.. % -target -value) 10)]
                                    (if (js/isNaN v) "" v)))}]]]

     [:label.flex.flex-col.gap-1
      [:span.text-sm.font-medium.opacity-70 (t :agent.settings/base-url)]
      [:input.form-input
       {:value (or (get-field :base-url) "")
        :on-change #(set-field :base-url (.. % -target -value))}]]

     [:div.flex.flex-col.gap-1
      [:span.text-sm.font-medium.opacity-70 (t :agent.settings/protocol)]
      [:div.text-sm.opacity-70 (t :agent.settings/protocol-openai)]]

     [:label.flex.flex-col.gap-1
      [:span.text-sm.font-medium.opacity-70 (t :agent.settings/api-key)]
      (when (true? (:has-api-key? base))
        [:span.text-xs.opacity-50 (t :agent.settings/api-key-saved)])
      [:input.form-input
       {:type (if key-visible? "text" "password")
        :value (or (get-field :api-key) "")
        :placeholder (t :agent.settings/api-key-placeholder)
        :autocomplete "new-password"
        :on-change #(set-field :api-key (.. % -target -value))}]]

     [:div.flex.flex-col.gap-1
      [:div.flex.items-center.justify-between
       [:span.text-sm.font-medium.opacity-70 (t :agent.settings/models)]
       [:button.text-sm
        {:type "button"
         :on-click #(set-field :models (conj models' {:name "" :enabled true}))}
        (t :agent.settings/add-model)]]
      (for [[idx m] (map-indexed vector models')]
        ^{:key idx}
        [:div.flex.items-center.gap-2
         [:input.form-input.flex-1
          {:value (model-name m)
           :placeholder (t :agent.settings/model-placeholder)
           :on-change #(set-field :models
                                    (assoc models' idx (assoc m :name (.. % -target -value))))}]
         (ui/toggle (not (false? (:enabled m)))
                    #(set-field :models
                                  (assoc models' idx
                                         (update m :enabled (fn [v] (not (false? v))))))
                    true)
         [:button.text-sm.opacity-60.hover:opacity-100
          {:type "button"
           :on-click #(set-field :models (remove-nth models' idx))}
          (t :agent.settings/delete-model)]])]

     (when (seq (:models base))
       [:div.text-xs.opacity-50 (t :agent.settings/models-replace-hint)])

     [:div.flex.gap-2
      (ui/button (if saving? (t :agent.settings/saving) (t :agent.settings/save))
                 :small? true :intent "primary"
                 :disabled (not can-save?)
                 :on-click save!)
      (ui/button (t :agent.settings/cancel) :small? true :on-click on-cancel)]]))

(hsx/defc provider-card
  [{:keys [provider agent-state editing-id draft key-visible* key-visible?
           get-field set-field on-cancel open-editor-for! refresh!]}]
  (let [editing? (= editing-id (:id provider))
        active-provider (:active-provider agent-state)
        active? (= (:id provider) active-provider)
        active-model (or (:active-model provider)
                         (when active? (:active-model agent-state)))
        models' (or (:models provider) [])
        label (or (:display-name provider) (t :agent.settings/provider))]
    [:div.cp__agent-provider-card.flex.flex-col.gap-2.p-3.rounded-md.bordered
     [:div.flex.items-center.justify-between.gap-2
      [:div.flex.items-center.gap-2.min-w-0
       (when active?
         [:span.text-xs.px-1.rounded.bg-base-hover (t :agent.settings/active)])
       [:div.font-medium.truncate label]]
      [:div.flex.items-center.gap-2
       (ui/toggle (not (false? (:enabled provider)))
                  (fn []
                    (p/let [_ (ipc/ipc :agent/upsert-provider
                                       {:id (:id provider)
                                        :enabled (not (false? (:enabled provider)))})]
                      (refresh!)))
                  true)
       (ui/button (if editing? (t :agent.settings/cancel) (t :agent.settings/edit))
                  :small? true :class "p-1"
                  :on-click (fn []
                              (if editing?
                                (on-cancel)
                                (open-editor-for! provider))))
       (ui/button (t :agent.settings/delete)
                  :small? true :class "p-1"
                  :on-click #(p/let [_ (ipc/ipc :agent/remove-provider (:id provider))]
                              (on-cancel)
                              (refresh!)))]]

     [:div.text-xs.opacity-60.font-mono.truncate (:base-url provider)]
     (when (seq models')
       [:div.text-xs.opacity-60
        (str (t :agent.settings/models) ": "
             (string/join ", " (mapv model-label models')))])
     (when (and active? (seq active-model))
       [:div.text-xs.opacity-60 (str (t :agent.settings/current-model) ": " active-model)])

     (when (and (seq models') (not editing?))
       (ui/button (t :agent.settings/set-current)
                  :small? true :class "self-start"
                  :on-click (fn []
                              (when-let [nm (or active-model
                                                (some #(when (not (false? (:enabled %)))
                                                        (:name %))
                                                      models'))]
                                (p/let [_ (ipc/ipc :agent/set-active (:id provider) nm)]
                                  (refresh!))))))

     (when editing?
       [provider-editor {:base provider
                         :draft draft
                         :key-visible* key-visible*
                         :key-visible? key-visible?
                         :get-field get-field
                         :set-field set-field
                         :on-cancel on-cancel
                         :on-saved (fn [] (on-cancel) (refresh!))}])]))

(hsx/defc preset-row
  [{:keys [preset selected-id on-pick!]}]
  (let [{:keys [id display-name base-url] :as preset'} preset]
    [:button.text-left.text-sm.flex.items-center.justify-between.gap-2.px-1.py-1.rounded-md.hover:bg-base-hover
     {:type "button"
      :class (when (= id selected-id) "bg-base-hover")
      :on-click #(on-pick! (if (= id selected-id) nil preset'))}
     [:span display-name]
     (when (= id selected-id)
       [:span.text-xs.opacity-60.font-mono base-url])]))

(hsx/defc preset-category
  [{:keys [category items selected-id on-pick!]}]
  [:div.flex.flex-col.gap-1
   [:div.text-xs.font-medium.opacity-50 (category-title category)]
   (for [{:keys [id] :as preset} items]
     ^{:key id}
     [preset-row {:preset preset :selected-id selected-id :on-pick! on-pick!}])])

(hsx/defc add-picker
  "Preset catalog grouped by category, mirroring SiYuan's provider list.
   Picking a preset installs it, then opens the new card's editor."
  [presets set-picking! refresh! open-editor-for!]
  (let [selected* (hooks/use-memo #(atom nil) [])
        [selected] (hooks/use-atom selected*)]
    [:div.cp__agent-add-picker.flex.flex-col.gap-3.p-3.rounded-md.bordered
     (for [[category items] (group-presets presets)]
       ^{:key (name category)}
       [preset-category {:category category
                         :items items
                         :selected-id (:id selected)
                         :on-pick! #(reset! selected* %)}])
     [:div.flex.gap-2
      (ui/button (t :agent.settings/confirm-add)
                 :small? true :intent "primary"
                 :disabled (nil? selected)
                 :on-click
                 (fn []
                   (p/let [{:keys [id]} selected
                           provider (ipc/ipc :agent/install-preset id)
                           _ (do (set-picking! false)
                                 (reset! selected* nil)
                                 (refresh!))]
                     (open-editor-for! provider))))
      (ui/button (t :agent.settings/cancel) :small? true
                 :on-click #(set-picking! false))]]))

(hsx/defc agent-settings
  []
  (let [agent-state* (hooks/use-memo #(atom nil) [])
        [agent-state] (hooks/use-atom agent-state*)
        picking* (hooks/use-memo #(atom false) [])
        [picking?] (hooks/use-atom picking*)
        editing* (hooks/use-memo #(atom nil) [])
        [editing-id] (hooks/use-atom editing*)
        draft* (hooks/use-memo #(atom {}) [])
        [draft] (hooks/use-atom draft*)
        key-visible* (hooks/use-memo #(atom false) [])
        [key-visible?] (hooks/use-atom key-visible*)
        get-field (fn [k] (get draft k))
        set-field (fn [k v] (swap! draft* assoc k v))
        refresh! (fn []
                   (p/let [s (ipc/ipc :agent/state)]
                     (reset! agent-state* s)
                     s))
        open-editor-for! (fn [provider]
                           (reset! key-visible* false)
                           (reset! draft* (draft-defaults provider))
                           (reset! editing* (:id provider)))
        on-cancel #(reset! editing* nil)]
    (hooks/use-effect!
     (fn []
       (when (util/electron?)
         (refresh!))
       nil)
     [])
    (when (seq agent-state)
      (let [{:keys [enabled? providers presets]} agent-state]
        [:div.cp__agent-settings.flex.flex-col.gap-4.mb-8
         [:div.flex.items-center.gap-3
          (ui/toggle enabled?
                     #(p/let [_ (ipc/ipc :agent/set-enabled (not enabled?))]
                        (refresh!)))
          [:div.text-sm.font-medium (t :agent.settings/enable-agent)]]

         [:div.flex.flex-col.gap-3
          [:div.flex.items-center.justify-between
           [:div.text-sm.font-medium (t :agent.settings/providers)]
           (ui/button (t :agent.settings/add-provider)
                      :small? true :class "p-1"
                      :disabled picking?
                      :on-click #(reset! picking* true))]

          (when picking?
            [add-picker presets #(reset! picking* %) refresh! open-editor-for!])

          (for [{:keys [id] :as provider} providers]
            ^{:key id}
            [provider-card {:provider provider
                            :agent-state agent-state
                            :editing-id editing-id
                            :draft draft
                            :key-visible* key-visible*
                            :key-visible? key-visible?
                            :get-field get-field
                            :set-field set-field
                            :on-cancel on-cancel
                            :open-editor-for! open-editor-for!
                            :refresh! refresh!}])]]))))
