(ns scada-ui.pages.modes-page
  (:require [clojure.string :as str]
            [goog.date.duration :as gdd]
            [reagent.core :as reagent]
            [re-frame.core :refer [reg-event-db reg-event-fx path reg-sub dispatch subscribe]]
            [reagent.interop :refer [$]]
            [reagent.format :refer [format]]            
            [cljs-time.core :as time]
            [cljs-time.coerce :as tc]
            [cljs-time.format :as tf]
            cljs-time.extend ;; to enable intuitive equality of date-time instances
            [scada-ui.pages.common :as pc]
            [scada-ui.state :as state]
            [scada-ui.format :as f]
            [scada-ui.components.bootstrap :as bs]
            [scada-ui.components.calendar :as cal]
            [scada-ui.components.park :as park]
            [scada-ui.components.table :as table]
            [scada-ui.remote :as remote]
            [scada-ui.i18n :refer [i18n]]
            cljsjs.d3
            [scada-ui.util.transit :as tr]))

(def color-classes ["Indicator" "YlGn", "YlGnBu", "GnBu", "BuGn", "PuBuGn", "PuBu", "BuPu", "RdPu", "PuRd", "OrRd", "YlOrRd", "YlOrBr", "Purples", "Blues", "Greens", "Oranges", "Reds", "Greys", "PuOr", "BrBG", "PRGn", "PiYG", "RdBu", "RdGy", "RdYlBu", "Spectral", "RdYlGn"])


(def default-state {:date state/today
                    :errors {}
                    :modes {:selected nil
                            :overall-history {}
                            :per-plant-history {}}
                    :plant -1
                    :color-class "RdYlGn"})

(def until-today ["2014-01-01" (f/date-time->str state/today)])

(state/assoc-in-handler :modes/set-modes [:modes-page :modes :selected])
(state/assoc-in-handler :modes/set-date [:modes-page :date])
(state/assoc-in-handler :modes/color-class [:modes-page :color-class])
(state/assoc-in-handler :modes/overall-history-loaded [:modes-page :modes :overall-history])
(state/assoc-in-handler :modes/set-plant [:modes-page :plant])
(state/assoc-in-handler :modes/errors-loaded [:modes-page :errors])

;; {plant {date {mode duration-in-seconds}}}
(reg-event-db :modes/per-plant-history-loaded [(path :modes-page :modes :per-plant-history)]
              (fn [m [_ m']]
                (merge-with merge m m')))


;; load aggregated time in modes per day for this plant if plant changed and we don't yet have data for all dates
(reg-event-fx :modes/select-plant
                  (fn [{:keys [db]} [_ plant]]
                    (let [m (:modes-page db)
                          plant-name (when (not= -1 plant) (:name (nth (:plants db) plant)))
                          fetching? (and plant-name (not= (:plant m) plant))] ;; TODO avoid reloading if we already have the data
                      {:db (assoc-in db [:modes-page :plant] plant)
                       :remote (when fetching?
                                 {:url "/db/modes/individual"
                                  :params {:dates until-today
                                           :plants [plant-name]}
                                  :success [[:modes/per-plant-history-loaded]
                                            [:modes/set-plant plant]]})})))
;; fetch calendar data for the complete park history
(reg-event-fx :modes/initialize (path :modes-page :modes)
              (fn [{:keys [db]} [_]] 
                {:db db
                 :remote (when (empty? (:overall-history db))
                           {:url "/db/modes/aggregated" :params {:dates until-today} :success [:modes/overall-history-loaded]})}))

;; new date selected: check and fetch missing daily errors and modes for all plants
(reg-event-fx :modes/change-date  ;; load data for this date if not cached
              (fn [{:keys [db]} [_ date]]
                (let [dates [(f/date-time->str date)
                             (f/date-time->str (time/plus (tc/from-date date) (time/days 1)))]
                      plant-names (map :name (:plants db))
                      plants-to-load (remove (fn [plant] (get-in db [:modes-page :modes :per-plant-history plant date])) plant-names)
                      load-errors? (not (get-in db [:errors date]))
                      load-daily-overview? (not-empty plants-to-load)]
                  ;; if we need to load data, delay changing the date to avoid flashes when trying to render yet unavailable data
                  {:db (if (not (or load-errors? load-daily-overview?))
                         (assoc-in db [:modes-page :date] date)
                         db)
                   :remote [(when load-errors? ;; load daily errors
                              {:url "/db/errors"
                               :params {:plants plant-names
                                        :dates dates}
                               :success [[:modes/errors-loaded]
                                         [:modes/set-date date]]})
                            ;; for each plant that has not yet a modes map for this date, load them now
                            (when load-daily-overview?
                              {:url "/db/modes/individual"
                               :params {:dates dates
                                        :plants plants-to-load}
                               :success [[:modes/per-plant-history-loaded]
                                         [:modes/set-date date]]})]})))

(state/reg-sub :modes/state [:modes-page])
(state/reg-sub :modes/raw-errors [:modes-page :errors])
(state/reg-sub :modes/stats [:modes-page :modes :per-plant-history])

(reg-sub :modes/errors
         :<- [:modes/state]
         :<- [:all-plant-names]
         :<- [:hydrated-errors [:modes/raw-errors]]
         (fn [[{plant-idx :plant} plant-names errors]]
           (let [all? (< plant-idx 0)
                 p-name (if (not all?) (nth plant-names plant-idx) "Park")]
             (->> errors
                  (filter #(or all? (= (:plant %) p-name)))
                  (sort-by (juxt :plant :timestamp))
                  vec))))

(reg-sub :modes/selected-modes
         :<- [:production-modes]
         :<- [:modes/state]
         (fn [[production-modes {:keys [modes]}]]
           (or (:selected modes) production-modes)))

(reg-sub :modes/settings
         :<- [:modes/selected-modes]
         :<- [:modes/state]
         (fn [[selected-modes {:keys [date plant color-class]}]]
           {:date date
            :plant plant
            :color-class color-class
            :current-modes selected-modes}))

(reg-sub :modes/calendar-data
         :<- [:all-plant-names]
         :<- [:modes/state]
         :<- [:modes/selected-modes]
         (fn [[plant-names state selected-modes]]
           (let [selected-plant (let [idx (:plant state)]
                                  (when (not= -1 idx)
                                    (nth plant-names idx)))
                 data (if selected-plant
                        (get-in state [:modes :per-plant-history selected-plant])
                        (get-in state [:modes :overall-history]))]
             (reduce-kv (fn [res date m] (assoc res date (apply + (map m selected-modes)))) {} data))))


(defn link-to [date]
  (str (when date (f/human-date date))))
;;;;;;;;;;;; availability/modes charts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn color-legend-dropdown [callback date operation-modes]
  [bs/DropdownButton {:title (str (i18n [:modes-page/mode]) "/" (i18n [:modes-page/legend]))
                      :id "legend"
                      :onSelect (fn [key] (when callback (callback key)))}
   (doall
    (for [[id {:keys [name color]}] operation-modes]
      ^{:key name}
      [bs/MenuItem {:id name
                    :eventKey id}
       [pc/color-cube color]
       name]))])

(defn color-legend [operation-modes]
  [:div
   (for [{:keys [name color]} (sort-by (comp :name second) (vals operation-modes))]
     ^{:key name}
     [:div [pc/color-cube color {:size 20}]
      name])])


(defn modes-table [modes operation-modes]
  (let [dummy (time/now)]
    [:table {:class "table table-compact table-striped"}
     [:thead
      [:tr
       [:th (i18n [:modes-page/mode])]
       [:th (i18n [:time/duration])]]]
     [:tbody
      (doall
       (for [[mode duration-percent] (reverse (sort-by second modes))
             :let [{:keys [name color]} (get operation-modes mode)]]
         ^{:key mode}
         [:tr
          [:td [pc/color-cube color] name]
          [:td (table/duration-str (time/interval dummy (time/plus dummy (time/seconds (* duration-percent 24 60 60)))))]]))]]))

(defn plant-select [plants plant]
  [:div.form-inline {:style {:display "inline-block"}}
   [:div.form-group
    [:select.form-control
     {:value plant
      :on-change #(let [idx (.. % -target -selectedIndex)]
                    (dispatch [:modes/select-plant (dec idx)]))}
     (map-indexed (fn [i plant] ^{:key i}
                    [:option plant]) (cons "Park" plants))]]])


(defn pie-chart
  "Pie chart with label at the lower right."
  [data plant-name x y inner-radius outer-radius operation-modes]
  (let [modes (keys data)
        durations (vals data)
        arc-fn (doto (js/d3.arc)
                     ($ innerRadius inner-radius)
                     ($ outerRadius outer-radius))]
    [:g.pie
     [:text {:dx "-14px"
             :dy "-3px"
             :transform (format "translate(%f,%f)" (+ x outer-radius 5) (+ y outer-radius 5))}
      plant-name]
     (when (not-empty durations)
       (doall (map-indexed
               (fn [idx d]
                 (let [mode (nth modes idx)
                       {:keys [name color]} (get operation-modes mode)]
                   ^{:key (str plant-name idx)}
                   [:path {:d (arc-fn d)
                           :fill (or color "grey")
                           :transform (str "translate(" x "," y ")")}
                    [:title (format "%s: %.2f%%" name (* 100 ($ d :value)))]]))
               ((js/d3.pie) (clj->js durations)))))]))

(defn mode-statistics-page []
  (let [operation-modes (subscribe [:operation-modes])
        settings (subscribe [:modes/settings])
        stats (subscribe [:modes/stats])
        calendar-data (subscribe [:modes/calendar-data])
        mode-callback #(dispatch [:modes/set-modes [(str %)]])
        plant-names (subscribe [:all-plant-names])
        plant-coords (subscribe [:plant-coordinates])]        
    (let [{:keys [date current-modes color-class plant]} @settings
          day-before (time/minus date (time/days 1))
          day-after (time/plus date (time/days 1))
          calendar-opts {;:extent [0 1]
                         :cell-size 14
                         :hide-zero? (= color-class "Indicator")
                         :highlight-day date
                         :color-class color-class
                         :callback #(dispatch [:navigate (link-to %)])
                         :tooltip-formatter (fn [date value] (format "%s: %.2f%%" (f/human-date date) (* 100 (or value 0))))}
          mode-name (str/join "/" (map (comp :name @operation-modes) current-modes))
          individual? (>= plant 0)
          plant (if individual? (nth @plant-names plant) "Park")]
      [pc/page
       [:div.container-fluid
        [:div.row
         [:h1.col-md-4 (i18n [:modes-page/title])]
         [:h2.col-md-6
          [:a {:class "btn btn-default"
               :href (link-to day-before)} "<<"]
          (str plant " - " (f/human-date date))                
          [:a {:class "btn btn-default"
               :href (link-to day-after)} ">>"]]
         [:div.col-md-2.form-group
          [:label (i18n [:modes-page/color-scale])]
          [:select.form-control.input-sm
           {:value color-class
            :on-change #(let [idx (.. % -target -selectedIndex)]
                          (dispatch [:modes/color-class (nth color-classes idx)]))}
           (map-indexed (fn [i clz] ^{:key i} [:option clz]) color-classes)]]]
        [:div.row
         [:div.col-md-6           
          [:div.row.shadowed
           [:h3 (i18n [:modes-page/choose]) [plant-select @plant-names plant]]
           [park/park-renderer (fn [[x y] idx]
                                 (let [plant-name (nth @plant-names idx)
                                       data (into (sorted-map) (get-in @stats [plant-name date]))]
                                   (pie-chart data plant-name x y 5 25 @operation-modes)))
            {:width 700
             :height 800
             :margin [30 30 60 30]
             :coordinates @plant-coords
             :callback #(dispatch [:modes/select-plant %])}]
           [:div [color-legend @operation-modes]]]]
         (when-let [cal-data @calendar-data]             
           [:div.col-md-6              
            [:div.row.shadowed
             [:h3 (i18n [:modes-page/per-day] [mode-name]) [:div.pull-right [color-legend-dropdown mode-callback date @operation-modes]]]
             [cal/calendar cal-data calendar-opts]]
            [:div.row.shadowed
             [:h3 (i18n [:modes-page/errors])]
             [table/error-table ::errors [:modes/errors]]]
            (when individual?
              [:div.row.shadowed
               [:h3 (i18n [:modes-page/durations])]
               [modes-table (into (sorted-map) (get-in @stats [plant date])) @operation-modes]])])]]])))
