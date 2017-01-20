(ns scada-ui.pages.charts-page
  (:require [clojure.string :as str]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [reg-event-db path reg-sub dispatch subscribe]]
            [reagent.format :refer [format]]            
            [cljs-time.core :as time]
            [cljs-time.coerce :as tc]
            [scada-ui.pages.common :as pc]
            [scada-ui.state :as state]
            [scada-ui.format :as f]
            [scada-ui.components.date-picker :refer [date-range-picker]]
            [scada-ui.components.bootstrap :as bs]
            [scada-ui.components.park :as park]
            [scada-ui.components.tree :refer [tree]]
            [scada-ui.components.chart :as chart]
            [scada-ui.i18n :refer [i18n]]
            [scada-ui.components.powercurve :as power]
            [scada-ui.components.anomalies :as anomalies]
            [scada-ui.components.table :as tables]
            [scada-ui.util.tree :refer [by-attribute]]))

(def default-state (let [last-month (time/minus state/today (time/months 1))]
                     {:flags {:errors? false
                              :modes? false
                              :only-production? false
                              :rescale? false
                              :changes? false
                              :line-only? false
                              :maps-like? true 
                              :grey? false
                              :log? true
                              :inverse? false}
                      :visible {:heatmap? false
                                :chart? true
                                :errors? true
                                :modes? true
                                :changes? true}
                      :hidden {}
                      :zoom {:start nil
                             :end nil
                             :values {}
                             :traces {}}
                      :overall {:start last-month
                                :end state/today
                                :values {}}
                      :modes []
                      :errors []
                      :changes []}))

(defn trigger-resize! []
  (if (.-fireEvent js/document)
    ;; IE11
    (.fireEvent js/document "onresize")
    ;; everybody else
    (let [evt (doto (.createEvent js/window.document "UIEvents")
                     (.initUIEvent "resize" true false js/window 0))]
           (.dispatchEvent js/window evt))))

(reg-event-db :charts/toggle (path [:chart :flags])
              (fn [db [_ key]]
                (update-in db [key] not)))

(reg-event-db :charts/toggle-visibility (path [:chart :visible])
              (fn [db [_ key]]
                ;; we use panels with variable widths. trigger artificial window resize event
                ;; to give width dependant components a chance to get informed about their new dimension
                (js/setTimeout trigger-resize! 100) 
                (update-in db [key] not)))

(reg-event-db :charts/set-time-interval (path [:chart])
              (fn [db [_ {:keys [start end]}]]
                (-> db
                    (assoc-in [:overall :start] start)
                    (assoc-in [:overall :end] end)
                    (assoc-in [:zoom :start] nil)
                    (assoc-in [:zoom :end] nil))))

(state/reg-sub :charts-page/chart-state [:chart])
(state/reg-sub :charts-page/visibility [:chart :visible])
(state/reg-sub :charts-page/flags [:chart :flags])

(def ranges [[[[:time/last-week "Letzte Woche"]] (time/minus state/today (time/weeks 1))]
             [[[:time/last-month "Letzter Monat"]] (time/minus state/today (time/months 1))]
             [[[:time/last-n-months "Letzte %1 Monate"] [3]] (time/minus state/today (time/months 3))]
             [[[:time/last-n-months "Letzte %1 Monate"] [6]] (time/minus state/today (time/months 6))]
             [[[:time/last-year "Letztes Jahr"]]  (time/minus state/today (time/years 1))]])

(defn chart-dropdown [{:keys [visible? errors? modes? changes? only-production? rescale? line-only? single-plant? maps-like?]}]
  [bs/SplitButton {:id "chartoptions"
                   :title "Chart"
                   :class (when visible? "active")
                   :on-click #(dispatch [:charts/toggle-visibility :chart?])
                   :on-select #(let [kw (keyword %)]
                                    (when (or (not= kw :modes?) single-plant?)
                                      (dispatch [:charts/toggle kw])))}
   [bs/MenuItem {:id :errors? :event-key :errors? }
    [:span {:title (i18n [:charts/error-tooltip "Zeige Fehlerzeiträume aller ausgewählter Anlagen"])}
     [:input {:type "checkbox" :checked errors? :read-only true}] (i18n [:errors "Fehler"])]]
   [bs/MenuItem {:id :modes? :event-key :modes? }
    [:span {:title (i18n [:charts/modes-tooltip "Zeige alle Operationsmodi, nur möglich für eine einzelne Anlage"])}
     [:input {:type "checkbox" :checked modes? :read-only true
              :disabled (not single-plant?)}] (i18n [:mode "Betriebsmodus"])]]
   [bs/MenuItem {:id :changes? :event-key :changes? }
    [:span {:title (i18n [:charts/parameters-tooltip "Zeige alle manuellen Parameterändeungen"])}
     [:input {:type "checkbox" :checked changes? :read-only true}] (i18n [:charts/parameters "Parameter"])]]
   [bs/MenuItem {:id :only-production? :event-key :only-production? }
    [:span {:title (i18n [:charts/production-tooltip "Zeige nur Daten in Produktionszeiträumen. Entspricht den Daten, die die Anomalieerkennung ausschließlich verarbeitet."])}
     [:input {:type "checkbox" :checked only-production? :read-only true}] (i18n [:charts/production "Nur Produktion"])]]
   [bs/MenuItem {:id :rescale? :event-key :rescale? }
    [:span {:title (i18n [:charts/compare-tooltip "Verändere Werte aller Serien so, dass sie optisch übereinander liegen."])}
     [:input {:type "checkbox" :checked rescale? :read-only true}] (i18n [:charts/compare "Vergleichen"])]]
   [bs/MenuItem {:id :line-only? :event-key :line-only? }
    [:span {:title (i18n [:charts/average-tooltip "Zeige nur Durchschnittswerte ohne Hinterlegung von Minimum/Maximum"])}
     [:input {:type "checkbox" :checked line-only? :read-only true}] (i18n [:charts/average "Nur Durchschnitt"])]]
   [bs/MenuItem {:divider true}]
   [bs/MenuItem {:header true} (i18n [:charts/reload "Neuladen erforderlich"])]
   [bs/MenuItem {:id :maps-like? :event-key :maps-like? }
    [:span {:title (i18n [:charts/zoom-tooltip "Zoom/Pan analog zu Google Maps"])}
     [:input {:type "checkbox" :checked maps-like? :read-only true}] (i18n [:charts/zoom-setting "Zoom per Mausrad"])]]])

(defn button-options [{:as opts :keys [errors? modes? changes? only-production? rescale? line-only? single-plant? visible?]}]
  (into [:div.btn-group
         [chart-dropdown opts]]
        (when visible?
          [[:button.btn.btn-default {:class (when errors? "active")
                                     :title (i18n [:charts/error-tooltip "Zeige Fehlerzeiträume aller ausgewählter Anlagen"])
                                     :on-click #(dispatch [:charts/toggle :errors?])} (i18n [:errors "Fehler"])]
           [:button.btn.btn-default {:class (when modes? "active")
                                     :title (i18n [:charts/modes-tooltip "Zeige alle Operationsmodi, nur möglich für eine einzelne Anlage"])
                                     :disabled (not single-plant?)
                                     :on-click #(dispatch [:charts/toggle :modes?])} (i18n [:mode "Betriebsmodus"])]
           [:button.btn.btn-default {:class (when changes? "active")
                                     :title (i18n [:charts/parameters-tooltip "Zeige alle manuellen Parameterändeungen"])
                                     :on-click #(dispatch [:charts/toggle :changes?])} (i18n [:charts/parameters "Parameter"])]
           [:button.btn.btn-default {:class (when only-production? "active")
                                     :title (i18n [:charts/production-tooltip "Zeige nur Daten in Produktionszeiträumen. Entspricht den Daten, die die Anomalieerkennung ausschließlich verarbeitet."])
                                     :on-click #(dispatch [:charts/toggle :only-production?])} (i18n [:charts/production "Nur Produktion"])]
           [:button.btn.btn-default {:class (when rescale? "active")
                                     :title (i18n [:charts/compare-tooltip "Verändere Werte aller Serien so, dass sie optisch übereinander liegen."])
                                     :on-click #(dispatch [:charts/toggle :rescale?])} (i18n [:charts/compare "Vergleichen"])]
           [:button.btn.btn-default {:class (when line-only? "active")
                                     :title (i18n [:charts/average-tooltip "Zeige nur Durchschnittswerte ohne Hinterlegung von Minimum/Maximum"])
                                     :on-click #(dispatch [:charts/toggle :line-only?])} (i18n [:charts/average "Nur Durchschnitt"])]])))

(reg-event-db :dates/adjust-time-interval
   (fn [db [_ path {:keys [start end]}]]
     (update-in db path assoc :start (time/earliest start end) :end (time/latest start end))))


(defn chart-options [chart-state selected-plants]
  (let [plants (str/join ", " selected-plants)
        single-plant? (= 1 (count selected-plants))
        date-range (str (f/human-date (-> @chart-state :overall :start)) " - " (f/human-date (-> @chart-state :overall :end)))
        flags (:flags @chart-state)
        visible? (:chart? (:visible @chart-state))]
    [:div
     [:div.form-inline
      [button-options (assoc flags :single-plant? single-plant? :visible? visible?)]
      [:div.btn-group.pull-right
       [:div.input-group
        [bs/DropdownButton {:title (i18n [:time/time "Zeit"])
                            :id "daterange"
                            :onSelect (fn [idx] (dispatch [:charts/set-time-interval {:start (get-in ranges [idx 1])
                                                                                     :end state/today}]))}
         (doall (map-indexed (fn [idx [[v args]]] ^{:key idx}
                               [bs/MenuItem {:id idx
                                             
                                             :href ""
                                             :eventKey idx} (i18n v args)]) ranges))]]
       [:div.input-group
        [bs/popup {}
         (i18n [:calendar "Kalender"])
         [date-range-picker (state/cursor chart-state [:overall] [:dates/adjust-time-interval [:chart :overall]])]]]]]
     (if (empty? plants)
       [:b (i18n [:charts/select-hint "Bitte über der Baumansicht oben Anlagen auswählen."])]
       [:div.row               
        [:span.col-md-4
         [:label (i18n [:time/period "Zeitraum"]) ":"] date-range]
        [:span
         [:label (if (= 1 (count plants)) (i18n [:plant "Anlage"]) (i18n [:plants "Anlagen"])) ": "] plants]])]))


(defn panel [header body]
  [:div.panel.panel-default
   [:div.panel-heading header]
   [:div.panel-body body]])

(defn table-panel [header table]
  [:div.panel.panel-default
   [:div.panel-heading header]
   table])

(defn generic-table [m value-fns]
  [:table.table [:thead>tr [:th "Key"] [:th "Value"]]
   [:tbody
    (doall (for [[k v] m :let [v (if (= :children k) (count v) v)
                               value-fn (get value-fns k str)]]
             ^{:key k}
             [:tr
              [:td k]
              [:td (if (map? v) [generic-table v value-fns] (value-fn v))]]))]])

(defn active-node-panel []
  (let [{t :type l :label :as node} @(subscribe [:tree/active-node])]
    [panel [:h5 (i18n [:selection "Auswahl: \"%1\""] [l])]
     (case t
       :power [power/powercurve-form (:options node) #(dispatch [:add-powercurve-config (:plant node) %])]
       :lasso-config [anomalies/options-form (:options node) #(println "TODO submit new lasso options:" %)]
       :lasso [tables/lasso-table node]
       :sensor [generic-table node {:point-type #(str/join ", " (map name %))}]
       [generic-table node {}])]))

(defn home-page []
  (let [selected-plants (subscribe [:selected-plants])
        chart-state (subscribe [:charts-page/chart-state])
        visibility (subscribe [:charts-page/visibility])
        flags (subscribe [:charts-page/flags])
        sel-series (subscribe [:selected-series])]
    [pc/scroll-position-aware :charts-page
     (fn []
       (let [{:keys [heatmap? chart? modes? errors? changes?]} @visibility
             data? (not-empty @sel-series)
             btn-classes "btn navbar-toggle"]
         [pc/page
          [:div.container-fluid
           [:div.row.bg
            [:div.col-sm-3
             [:div.panel.panel-default
              [:div.panel-body
               [tree [:tree] {:mode :select}]]]]
            [:div.col-sm-9
             [:div.row
              [:div {:class (cond (not chart?) "col-md-2"
                                  (and chart? heatmap?) "col-md-6"
                                  :else "col-md-10")}
               [panel
                [chart-options chart-state (mapv :name @selected-plants)]
                (when chart? [chart/remote-line-chart])]]
              [:div {:class (cond (not heatmap?) "col-md-2"
                                  (and heatmap? chart?) "col-md-6"
                                  :else "col-md-10")}
               [panel
                [bs/SplitButton {:title "Heatmap"
                                 :id "heatmap-toggle"
                                 :class (when heatmap? "active")
                                 :on-click #(dispatch [:charts/toggle-visibility :heatmap?])
                                 :on-select #(dispatch [:charts/toggle (keyword %)])}
                 (doall
                  (for [[k lbl] [[:grey? (i18n [:charts/grey "Graustufen"])]
                                 [:inverse? (i18n [:charts/negative "Negativ"])]
                                 [:log? (i18n [:charts/logarithmic "Logarithmisch"])]]]
                    ^{:key k}
                    [bs/MenuItem {:id k :event-key k}
                     [:span [:input {:type "checkbox" :checked (boolean (get @flags k)) :read-only true}] lbl]]))]
                (when heatmap? [chart/remote-heatmap])]]]
             (when (and data? chart?)
               [:div.row
                [:div.col-md-6
                 [table-panel [:h4 (i18n [:charts/error-table-header "Fehlermeldungen SCADA"])]
                  [chart/errors-table]]]
                [:div.col-md-6
                 [table-panel [:h4 (i18n [:charts/settings-table-header "Einstellungsänderungen"])]
                  [chart/changes-table]]]])
             (when (and data? chart?)
               [:div.row
                [:div.col-md-12
                 [table-panel [:h4 (i18n [:charts/modes-table-header "Betriebszustände"])]
                  [chart/modes-table]]]])
             [:div.row
              [:div.col-md-12
               [active-node-panel]]]]]]]))]))
