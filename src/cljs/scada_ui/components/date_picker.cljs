(ns scada-ui.components.date-picker
  (:require cljsjs.react-day-picker
            [reagent.core :as reagent]
            [scada-ui.i18n :refer [i18n]]
            [cljs-time.core :as time]
            [cljs-time.coerce :as tc]))

;; --------------------- calendar to pick a date range -------------------------------------------------------------
(def js-day-picker (reagent/adapt-react-class js/DayPicker))


(defn- set-month [date month]
  (time/date-time (time/year date) month (time/day date)))

(defn- set-year [date year]
  (time/date-time year (time/month date) (time/day date)))

(defn- year-month [date]
  (let [current-year (time/year (time/now))
        select-style {:border 0
                      :width "auto"
                      :margin-top "-.35rem"
                      :margin-right "1rem"
                      :padding-right "15px"
                      :background-color "#FFF"
                      :appearance "none"
                      :-webkit-appearance "none"
                      :-moz-appearance "none"
                      :text-indent "1px"
                      :text-overflow ""}
        years (range current-year (- current-year 8) -1)]
    [:div {:class "DayPicker-Caption"}
     [:select {:name "month"
               :value (i18n [:time/months-long] [(dec (time/month @date))])
               :style select-style
               :on-change #(let [idx (.. % -target -selectedIndex)]
                             (swap! date set-month (inc idx)))}
      (for [month (i18n [:time/months])]
        ^{:key month}
        [:option {:value month :key month} month])]
     [:select {:name "year"
               :value (time/year @date)
               :style (assoc select-style :width "55px")
               :on-change #(let [idx (.. % -target -selectedIndex)]
                             (swap! date set-year (nth years idx)))}
      (for [year years]
        ^{:key year}
        [:option {:value year :key year} year])]]))

(defn day-picker [date]
  [:div {:style {:width "215px"}}
   [js-day-picker
    {:modifiers {:selected #(= (time/at-midnight @date) (time/at-midnight (tc/from-date %)))}
     :locale "de"
     :enable-outside-days false
     :number-of-months 1
     :initial-month (tc/to-date @date)
     :on-month-change #(reset! date (tc/to-date-time %))
     :locale-utils {:format-weekday-short #(i18n [:time/weekday-short] [%1])
                    :format-day #(str "to be implemented")
                    :get-first-day-of-week (constantly 1)
                    :format-weekday-long #(i18n [:time/weekday-long] [%1])}
     :on-day-click #(let [d (tc/from-date %2)]
                      (reset! date d))
     :caption-element (reagent/as-element [year-month date])}]])

(defn day-in-range-picker' [dates k]
  (reagent/with-let [initial-date (reagent/atom (get @dates k))]
    [:div {:style {:width "215px"}}
     [js-day-picker
      {:modifiers {:selected #(time/within? @dates (time/at-midnight (tc/from-date %)))
                   :disabled #(time/after? (tc/from-date %) (time/today))}
       :locale "de"
       :enable-outside-days false
       :number-of-months 1
       :initial-month (tc/to-date @initial-date)
       :on-month-change #(reset! initial-date (time/at-midnight (tc/to-date-time %)))
       :locale-utils {:format-weekday-short #(i18n [:time/weekday-short] [%1])
                      :format-day #(str "to be implemented")
                      :get-first-day-of-week (constantly 1)
                      :format-weekday-long #(i18n [:time/weekday-long] [%1])}
       :on-day-click #(let [d (tc/from-date %2)]
                        (when (not (time/after? d (time/today)))
                          (swap! dates assoc k d)))
       ;; TODO need to access props to distinguish between month/year to display and date selected
       :caption-element (reagent/as-element [year-month initial-date])}]]))

(defn date-range-picker
"Dates must be a cursor"
  [dates]
  {:pre [(satisfies? IDeref dates)]}
  [:div.container-fluid
   [:div.row
    [:h5.col-md-6 (i18n [:start])]
    [:h5.col-md-6 (i18n [:end])]]
   [:div.row
    [:div.col-md-6
     [day-in-range-picker' dates :start]]
    [:div.col-md-6
     [day-in-range-picker' dates :end]]]])
