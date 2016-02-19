(ns gt1-ui.pages.modes-page
  (:require [clojure.string :as str]
            [goog.date.duration :as gdd]
            [accountant.core :as accountant]
            [gt1-ui.bootstrap :as bs]
            [reagent.core :as reagent]
            [reagent.session :as session]
            [reagent.ratom :refer-macros [reaction]]
            [reagent.interop :refer-macros [$]]
            [reagent.format :refer [format]]            
            [cljs-time.core :as time]
            [cljs-time.coerce :as tc]
            [cljs-time.format :as tf]
            cljs-time.extend ;; to enable intuitive equality
            [gt1-ui.pages.common :as pc]
            [gt1-ui.calendar :as cal]
            [gt1-ui.park :as park]
            [gt1-ui.remote :as remote]
            [gt1-ui.defaults :refer [status-codes in-production color-classes]]
            [gt1-ui.table :as table]
            cljsjs.d3))

(defonce stats (reagent/atom {:stats {}
                              :date nil
                              :summed {}
                              :daily-averages {}
                              :modes in-production
                              :plant -1
                              :color-class "RdYlGn"}))

(defn link-to [date]
  (str "/availability/" (when date (pc/human-date date))))
;;;;;;;;;;;; availability/modes charts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn color-legend-dropdown [callback]
  [bs/DropdownButton {:title "Zustand/Legende"
                      :onSelect (fn [evt key] (when callback (callback key)))}
   (doall
    (for [[i {:keys [name color]}] (sort-by (comp :name second) status-codes)]
      ^{:key i}
      [bs/MenuItem {:id i
                    :href (link-to (:date @stats))
                    :eventKey i}
       [pc/color-cube color]
       name]))])

(defn color-legend []
  [:div
   (for [[i {:keys [name color]}] (sort-by (comp :name second) status-codes)]
     ^{:key i}
     [:div [pc/color-cube color {:size 20}]
      name])])



(defn daily-averages [summed modes]
  (into {}
        (for [[d m] summed
              :when m
              :let [sum-per-day (apply + (vals m))]
              :when (not (zero? sum-per-day))]
          [d (/ (apply + (map m modes)) sum-per-day)])))


(defn modes-table [modes]
  [:table {:class "table table-compact table-striped"}
     [:thead
      [:tr
       [:th "Betriebsmodus"]
       [:th "Dauer"]]]
     [:tbody
      (for [[mode duration-in-s] modes
            :let [{:keys [name color]} (get status-codes mode)]]
        ^{:key mode}
        [:tr
         [:td [pc/color-cube color] name]
         [:td (gdd/format (* duration-in-s 1000))]])]])

(defn plant-select [plants plant]
  [:form.form-inline
   [:div.form-group
    [:label "Bitte wählen Sie eine eine Anlage aus:"]
    [:select.form-control
     {:value plant
      :on-change #(let [idx (.. % -target -selectedIndex)]
                    (swap! stats assoc :plant (dec idx)))}
     (map-indexed (fn [i plant] ^{:key i}
                    [:option plant]) (cons "Park" @plants))]]])

(defn mode-statistics-page [crnt-date]
  (let [plants (reaction (mapv :name (session/get :plants)))        
        inner-radius 10
        outer-radius 30
        small-arc-fn (doto (js/d3.svg.arc)
                       ($ innerRadius 5)
                       ($ outerRadius 25))
        big-arc-fn (doto (js/d3.svg.arc)
                     ($ innerRadius 50)
                     ($ outerRadius 200))
        pie-chart (fn [date arc-fn idx x y]
                    (let [plant (nth @plants idx)                                  
                          data (into (sorted-map) (get-in @stats [:stats plant date])) ; {date {mode seconds}}
                          modes (keys data)
                          durations (vals data)]
                      [:g.pie
                       [:text {:dx "-14px"
                               :dy "-3px"
                               :transform (format "translate(%f,%f)" (+ x outer-radius) (+ y outer-radius))}
                        plant]
                       (when (not-empty durations)
                         (doall (map-indexed
                                 (fn [idx d]
                                   (let [mode (nth modes idx)
                                         {:keys [name color]} (get status-codes mode)]
                                     ^{:key (str plant idx)}
                                     [:path {:d (arc-fn d)
                                             :fill (or color "grey")
                                             :transform (str "translate(" x "," y ")")}
                                      [:title (format "%s: %.2f%%" name (* 100 (/ ($ d :value) 86400)))]]))
                                 ((js/d3.layout.pie) (clj->js durations)))))]))
        mode-callback #(swap! stats assoc
                              :modes [%]
                              :daily-averages (daily-averages (:summed @stats) [%]))]
    
    (when (empty? (:stats @stats))
      (remote/GET "/db/aggregated-modes"
          {:params {:dates ["2014-01-01"
                            (tf/unparse (tf/formatter "yyyy-MM-dd") (time/now))]}
           :handler #(let [summed (reduce (partial merge-with (partial merge-with +)) (vals %))];; {plant {date {mode duration-in-seconds}}}
                       (swap! stats assoc
                              :stats %
                              :summed summed
                              ;; calculate average duration of mode 'production' per day over all plants
                              :daily-averages (daily-averages summed (:modes @stats))))}))
    (fn [date]
      (let [{cc :color-class} @stats
            day-before (time/minus (tc/from-date date) (time/days 1))
            day-after (time/plus (tc/from-date date) (time/days 1))
            calendar-opts {;:extent [0 1]
                           :cell-size 14
                           :highlight-day date
                           :color-class cc
                           :callback #(accountant/navigate! (link-to %))
                           :tooltip-formatter (fn [date value] (format "%s: %.2f%%" (pc/human-date date) (* 100 (or value 0))))}
            mode-name (str/join "/" (map (comp :name status-codes) (:modes @stats)))
            idx (:plant @stats)
            individual? (>= idx 0)
            plant (if individual? (nth @plants idx) "Park")
            errors (:errors @stats)
            errors (if individual? (select-keys errors [plant]) errors)]
        (when (not= date (:date @stats))
          (swap! stats assoc :date date)
          (remote/GET "/db/data" {:params {:series []
                                           :plants @plants
                                           :values? false
                                           :errors? true
                                           :modes? false
                                           :dates [(pc/date-time->str date)
                                                   (pc/date-time->str day-after)]}
                                  :handler #(swap! stats assoc :errors (into (sorted-map) (:errors %)))}))
        [:div
         [pc/toolbar [color-legend-dropdown mode-callback]]
         [pc/loading-bar]
         [:div.container-fluid {:style {:background-color "white"}}
          [:div.row
           [:h1.col-md-10 "Gesamtübersicht pro Tag"]
           [:div.col-md-2.form-group
            [:label "Farbskala"]
            [:select.form-control.input-sm
             {:value cc
              :on-change #(let [idx (.. % -target -selectedIndex)]
                            (swap! stats assoc :color-class (nth color-classes idx)))}
             (map-indexed (fn [i clz] ^{:key i}
                            [:option clz]) color-classes)]]]
          [:h3 "Aktueller Tag:" (pc/human-date date)]          
          [:div.row
           [:div.col-md-6
            [plant-select plants plant]
            [park/park-renderer (fn [[x y] idx] (pie-chart date small-arc-fn idx x y))
             {:width 700
              :height 800
              :coordinates @pc/coords
              :callback #(swap! stats assoc :plant %)}]
            ;; move color legend slightly into the park rendering above
            [:div {:style {:margin-top -350}}
             [color-legend]]
            (when individual?
              [:svg.col-md-8 {:width 500 :height 500}
               (pie-chart date big-arc-fn idx 250 250)])]
           (when-let [dailies (:daily-averages @stats)]             
             (let [cal-data (if individual?
                              (into {} (for [[date m] (get-in @stats [:stats plant])]
                                         [date (/ (apply + (map m (:modes @stats))) 86400)]))
                              dailies)]
               [:div.col-md-6 
                [:div.row
                 [:h2
                  [:a {:class "btn btn-default"
                       :href (link-to day-before)}
                   '"<<"]
                  (str plant " - " (pc/human-date date))                
                  [:a {:class "btn btn-default"
                       :href (link-to day-after)}
                   ">>"]]
                 [:h3 (str mode-name " pro Tag")]
                 [cal/calendar cal-data calendar-opts]]
                [:div.row
                 [:h3 "Fehlermeldungen"]
                 (if (not-empty errors)
                   [table/error-table errors]
                   [:span.bg-success [:i {:class "glyphicon glyphicon-ok"}] "keine Fehler"])]
                (when individual?
                  [:div.row
                   [:h3 "Verweilzeiten in Betriebszustand"]
                   [modes-table (into (sorted-map) (get-in @stats [:stats plant date]))]])]))]]]))))
