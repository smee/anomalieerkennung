(ns gt1-ui.pages.charts-page
  (:require [clojure.string :as str]
            [gt1-ui.bootstrap :as bs]
            [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [reagent.ratom :refer-macros [reaction]]
            [reagent.format :refer [format]]            
            [cljs-time.core :as time]
            [cljs-time.coerce :as tc]
            [gt1-ui.pages.common :as pc :refer [at toggle!]]
            [gt1-ui.calendar :as cal]
            [gt1-ui.park :as park]
            [gt1-ui.chart :as chart]
            [gt1-ui.tree :refer [tree]]
            [reagent-forms.core :refer [bind-fields]]))


(defn- date-picker-date [dt]
  {:year (time/year dt)
   :month (time/month dt)
   :day (time/day dt)})

(defn chart-options [chart-state selected-plants]
  (let [today  (time/today-at-midnight)         
        ranges [["Freie Auswahl"]
                ["Letzte Woche" (date-picker-date (time/minus today (time/weeks 1)))]
                ["Letzter Monat" (date-picker-date (time/minus today (time/months 1)))]
                ["Letzte 3 Monate" (date-picker-date (time/minus today (time/months 3)))]
                ["Letzte 6 Monate"  (date-picker-date (time/minus today (time/months 6)))]
                ["Letztes Jahr"  (date-picker-date (time/minus today (time/years 1)))]
                ["Alle Daten"   (date-picker-date (time/minus today (time/years 10)))]]]
    [:div [:div.btn-toolbar 
           [:div.btn-group
            [:button.btn.btn-default {:class (when (:errors? @chart-state) "active")
                                      :title "Zeige Fehlerzeiträume aller ausgewählter Anlagen"
                                      :on-click #(swap! chart-state update-in [:errors?] not)} "Fehler"]
            [:button.btn.btn-default {:class (when (:modes? @chart-state) "active")
                                      :title "Zeige alle Operationsmodi, nur möglich für eine einzelne Anlage"
                                      :disabled (not= 1 (count @selected-plants))
                                      :on-click #(swap! chart-state update-in [:modes?] not)} "Zustand"]
            [:button.btn.btn-default {:class (when (:only-production? @chart-state) "active")
                                      :disabled (not= 1 (count @selected-plants))
                                      :title "Zeige nur Daten in Produktionszeiträumen. Entspricht den Daten, die die Anomalieerkennung ausschließlich verarbeitet."
                                      :on-click #(swap! chart-state update-in [:only-production?] not)} "Nur Produktion"]
            [:button.btn.btn-default {:class (when (:rescale? @chart-state) "active")
                                      :title "Verändere Werte aller Serien so, dass sie optisch übereinander liegen."
                                      :on-click #(swap! chart-state update-in [:rescale?] not)} "Vergleichen"]
            [:button.btn.btn-default {:class (when (:line-only? @chart-state) "active")
                                      :title "Zeige nur Durchschnittswerte ohne Hinterlegung von Minimum/Maximum"
                                      :on-click #(swap! chart-state update-in [:line-only?] not)} "Nur Durchschnitt"]]
           [:div.input-group {:style {:width "300px"}}
            [:span.input-group-addon "Zeitraum:"]
            [:select.form-control {:style {:width "150px"}
                                   :on-change #(let [idx (.. % -target -selectedIndex)]
                                                 (when (> idx 0)
                                                   (swap! chart-state assoc :overall {:start-date (get-in ranges [idx 1])
                                                                                      :end-date (date-picker-date today)})))}
             (doall (map-indexed (fn [idx [t]] ^{:key idx} [:option t]) ranges))]]]
     [bind-fields [:div.form-inline
                   [:div [:div.input-group
                          [:span.input-group-addon "von"]
                          [:input.form-control {:field :datepicker
                                                :id :start-date
                                                :date-format "dd.mm.yyyy"
                                                :lang :de-DE
                                                :inline true
                                                :auto-close? true
                                                :style {:width "120px"}}]]
                    [:div.input-group
                     [:span.input-group-addon "bis"]
                     [:input.form-control {:field :datepicker
                                           :id :end-date
                                           :date-format "dd.mm.yyyy"
                                           :lang :de-DE
                                           :inline true
                                           :auto-close? true
                                           :style {:width "120px"}}]]
                    [:span.bg-info
                     [:span {:class "glyphicon glyphicon-info-sign"}]
                     "Zum Zoomen bitte im Chart klicken und ziehen (horizontal oder vertikal)."]]]
      (reagent/cursor chart-state [:overall])]]))

(defn plant-selector []
  [:div {:style {:display (if (session/get-in [:show-plants?]) "block" "none")}}
   [:h1 "Auswahl der Anlagen"]
   [park/park-renderer (fn [[x y] idx]
                         (let [c (at :plants idx)
                               {:keys [name selected?]} @c]
                           [:g
                            [:circle {:cx x :cy y :r 8
                                      :fill (if selected? "blue" "white")
                                      :stroke "black"}]
                            [:text {:transform (format "translate(%f,%f)" (- x 8) (+ y 20))} name]]))
    {:width 600
     :height 600
     :coordinates @pc/coords
     :callback (fn [idx]
                 (toggle! [:plants idx :selected?]))}]])

(defn home-page []
  (let [tree-data (at :structure :tree)
        tree-state (at :tree)
        selected-plants (reaction (mapv :name (filter :selected? (session/get :plants))))
        selected-nodes (reaction (-> @tree-state :active :selected vals))
        chart-state (at :chart)
        btn-classes "btn glyphicon"]
    (fn []
      [:div 
       [pc/toolbar [:li
                 [:a {:class (if (session/get-in [:show-plants?]) (str btn-classes " active") btn-classes)
                      :title "Auswahl der Anlage und Zeitraum"
                      :on-click #(toggle! [:show-plants?])}
                  [:img {:src "images/wea-icon.svg"}]]]]
       [pc/loading-bar]
       [:div.container-fluid
        [:div.row 
         [:div.col-md-offset-6 {:style {:background-color "white"}}
          [plant-selector]]]
        [:div.row.bg
         [:div.col-md-3 {:style {:height "90vh"
                                 :overflow-y :auto}}
          [tree tree-data tree-state {:mode :select}]]
         [:div.col-md-9 {:style {:background-color "white"}}
          [:div.row           
           (let [plants (str/join "," @selected-plants)]
             (if (empty? plants)
               [:b {:class "bg-warning"} "Bitte rechts oben Anlagen auswählen."]
               [:span
                [:label "ausgewählt: "] plants]))]
          (when (not-empty @selected-plants)
            [:div.row
             [chart-options chart-state selected-plants]])
          [:div.row
           [chart/remote-line-chart @selected-plants @selected-nodes chart-state]]]]]])))
