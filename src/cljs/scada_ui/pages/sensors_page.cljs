(ns scada-ui.pages.sensors-page
  (:require [goog.dom :as dom]
            goog.dom.forms
            [cljs.reader :as reader]
            [re-frame.core :refer [reg-sub subscribe dispatch reg-event-db reg-event-fx path]]
            [scada-ui.pages.common :as pc]
            [scada-ui.state :as state]
            [scada-ui.util.tree :as tree]
            [scada-ui.i18n :refer [i18n]]
            [reagent.core :as r]
            [scada-ui.components.bootstrap :as bs]))

(def default-state {:editing? false
                    :limits {}})

(state/assoc-in-handler :sensors-page/received-limits [:sensors-page :limits])

(reg-event-fx :sensors-page/initialize
              (fn [{db :db}]
                (let [type (get-in db [:structure :name])
                      limits (get-in db [:sensors-page :limits])]
                  (when (empty? limits)
                    {:remote {:url "/db/limits"
                              :params {:type type}
                              :success [:sensors-page/received-limits]}}))))

(state/reg-sub :sensors-page/state [:sensors-page])

;; TODO store in backend, use for automatic monitoring
(reg-event-fx :sensors-page/store-limits
              (fn [{db :db} [_ limits]]
                {:db (assoc-in db [:sensors-page :limits] limits)
                 :remote (when (not= limits (get-in db [:sensors-page :limits]))
                           {:url "/db/limits"
                            :method :post
                            :params {:type (get-in db [:structure :name])
                                     :limits limits}})
                 :dispatch [:sensors-page/toggle-edit]}))

(state/toggle-handler :sensors-page/toggle-edit [:sensors-page :editing?])

;; ---------------------- sensors table ------------------------------------------------

(defn edit-in-place [ks v]
  [:input {:class "form-control"
           :type "number"
           :step :any
           :name (str ks)
           :default-value v}])

(defn sensors-page []
  (let [table (subscribe [:all-sensors])
        name (subscribe [:structure-name])        
        state (subscribe [:sensors-page/state])
        toggle-edit #(dispatch [:sensors-page/toggle-edit])]
    (fn []
      (let [{:keys [limits editing?]} @state]
        [pc/page
         [:h2 (str (count @table) " " (i18n [:sensors "Sensoren"]) " in " @name)
          [:div.pull-right
           (if editing?
             [:div
              [:button {:class "btn btn-default"
                        ;; TODO fetch form, traverse elements, build limits map
                        :on-click #(let [data (reduce (fn [m [s [v]]]
                                                        (if (not-empty v)
                                                       (assoc-in m (reader/read-string s) (js/parseFloat v))
                                                       m))
                                                      {}
                                                      (js->clj (.toObject (js/goog.dom.forms.getFormDataMap (aget js/document.forms 0)))))]
                                     (dispatch [:sensors-page/store-limits data]))}
               [bs/Glyphicon {:glyph "ok"}]]
              [:button {:class "btn btn-default"
                        :on-click #(dispatch [:sensors-page/toggle-edit])}
               [bs/Glyphicon {:glyph "remove"}]]]
             [:button {:class  (str "btn btn-default" (when editing? "active"))
                       :on-click toggle-edit}
              [bs/Glyphicon {:glyph "pencil"}]])]]
         (if (not table) 
           [:p (i18n [:please-wait "Bitte warten...."])]
           [:form#sensors-form
            [:table {:class "table table-condensed table-hover table-striped"}
             [:colgroup
              [:col {:style {:width "5%"}}]
              [:col {:style {:width "20%"}}]
              [:col {:style {:width "5%"}}]
              [:col {:style {:width "15%"}}]
              [:col {:style {:width "15%"}}]
              [:col {:style {:width "15%"}}]
              [:col {:style {:width "25%"}}]]
             [:thead
              [:tr
               [:th "ID"]
               [:th (i18n [:label "Bezeichnung"])]
               [:th (i18n [:unit "Einheit"])]
               [:th (i18n [:minimum "Minimum"])]
               [:th (i18n [:maximum "Maximum"])]
               [:th (i18n [:physical-dimension "Physikalische Größe"])]
               [:th (i18n [:description "Beschreibung"])]
               ]]
             [:tbody
              (doall
               (for [[idx {:keys [series unit label dimension description]}] (map-indexed vector @table)
                     :let [mi (get-in limits [series :min])
                           ma (get-in limits [series :max])]]
                 ^{:key idx}
                 [:tr
                  [:td series]
                  [:td label]
                  [:td unit]
                  [:td (if editing? [edit-in-place [series :min] mi] (when mi (str mi unit)))]
                  [:td (if editing? [edit-in-place [series :max] ma] (when ma (str ma unit)))]                       
                  [:td dimension]
                  [:td description]]))]]])]))))
