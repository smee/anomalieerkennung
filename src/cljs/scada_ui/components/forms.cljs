(ns scada-ui.components.forms
  (:require [clojure.set :as set]
            [cljs.reader :as reader]
            [goog.dom :as dom]
            [reagent.core :as reagent]
            [re-frame.core :refer [register-handler path register-sub dispatch subscribe]]
            [scada-ui.i18n :refer [i18n]]))

(defn subset-of-list
"Widget that presents two multiple-selects next to each other: On the left all available, unselected elements,
on the right the currently selected values. Two buttons in the middle allow to move elements betweeen these lists."
  [all-values selected-values
   {:keys [label-fn selection-changed-fn id-fn sorted? header-all header-selected]
    :or {id-fn identity, label-fn str, selection-changed-fn (fn [_])
         header-all (i18n [:forms/all-entries "Alle verfügbaren Einträge"])
         header-selected (i18n [:forms/selected "Ausgewählt"])}}]
  (let [evt-handler (fn [list-class add? evt]
                      (let [options (-> evt .-target (dom/getAncestorByClass "list-selector") (.getElementsByClassName list-class) (aget 0) .-options)
                            ids (set (keep #(when (.-selected %) (.-value %)) (array-seq options)))]
                        (selection-changed-fn add? ids)
                        (.preventDefault evt)                                               
                        false))
        unselected-values (set/difference (set all-values) (set selected-values))
        unselected-values (if sorted? (sort-by label-fn unselected-values) unselected-values)
        selected-values (if sorted? (sort-by label-fn selected-values) selected-values)]
    [:div.container-fluid.list-selector
     [:div.row
      [:div.col-md-6
       [:h6 header-all]
       [:select.form-control.left-list {:multiple true :size (min 10 (count unselected-values))}
        (for [v unselected-values :let [id (id-fn v)]]
          ^{:key id} [:option {:value id :title id} (label-fn v)])]]
      [:div.col-md-1
       [:h6 "\u00a0"] ;; add an empty heading so the button are on the same vertical position as the selects left and right
       [:div
        [:button.btn.btn-default {:title (i18n [:forms/deselect "Abwählen"])
                                  :on-click (partial evt-handler "right-list" false)}
         [:i {:class "glyphicon glyphicon-chevron-left"}]]
        [:button.btn.btn-default {:title (i18n [:forms/select "Auswählen"])
                                  :on-click (partial evt-handler "left-list" true)}
         [:i {:class "glyphicon glyphicon-chevron-right"}]]]]
      [:div.col-md-5
       [:h6 header-selected]
       [:select.form-control.right-list {:multiple true :size (min 10 (count selected-values))}
        (for [v selected-values :let [id (id-fn v)]]
          ^{:key id} [:option {:value id :title id} (label-fn v)])]]]]))

(defn input [state label input-opts {:keys [help-text prefix suffix show-help? path]}]
  ;; use form-2 to prevent recreating input fields all the time, would loose focus after each keypress otherwise!
  (fn [state label input-opts {:keys [help-text prefix suffix show-help? path]}]
    (let [value (get-in @state path)
          cb? (and (map? input-opts) (= "checkbox" (:type input-opts)))
          inp (cond
                cb? [:input.form-control
                     (assoc input-opts
                            :checked value
                            :on-change #(let [v (-> % .-target .-checked)]
                                          (swap! state assoc-in path v)))]
                (map? input-opts) [:input.form-control
                                   (assoc input-opts
                                          :value value
                                          :on-change #(when-let [v (try
                                                                     (reader/read-string (-> % .-target .-value))
                                                                     (catch js/Object o nil))]
                                                        (swap! state assoc-in path v)))]
                :else input-opts)]
      [:div.form-group
       [:label.control-label
        {:title help-text} label]
       [:div
        (if (or prefix suffix)
          [:div.input-group
           (when prefix [:div.input-group-addon prefix])
           inp
           (when suffix [:div.input-group-addon suffix])]
          inp)
        (when show-help? [:span.help-block help-text])]])))

(defn form-row [& columns]
  (let [col-width (min 12 (max 1 (int (/ 12 (count columns)))))
        class (str "col-md-" col-width)]
    (into [:div.row] (for [col columns] [:div {:class class} col]))))

(defn help-button
"Needs atom/cursor with key `:help?`. Shows/toggles this boolean value."
  [state]
  [:button.btn.pull-right.btn-default
   {:class (when (:help? @state) "active")
    :title (i18n [:forms/toggle-help "Hilfetexte ein-/ausblenden"])
    :on-click #(swap! state update-in [:help?] not)}
   [:i {:class "glyphicon glyphicon-question-sign"}]])
