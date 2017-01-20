(ns scada-ui.pages.wind-page
  (:require [scada-ui.components.park :as park]
            [scada-ui.remote :as remote]
            [scada-ui.format :as f]
            [scada-ui.pages.common :as pc]
            [scada-ui.format :as f]
            [scada-ui.state :as state]
            [scada-ui.components.date-picker :refer [date-range-picker]]
            [scada-ui.i18n :refer [i18n]]
            [reagent.core :as reagent]
            [cljs-time.core :as time]
            [re-frame.core :refer [reg-event-db path reg-sub dispatch subscribe]]))

(def default-state {:dates {:start (time/minus state/today (time/weeks 1))
                            :end state/today}
                    :data {}
                    :sector-angle 15})

(reg-event-db :wind/data-loaded (path [:wind-page :data]) (fn [db [_ response]] (merge db response)))
(state/assoc-in-handler :wind/plant-selected [:wind-page :plant])
(state/assoc-in-handler :wind/sector-angle [:wind-page :sector-angle])

(state/reg-sub :wind/wind-data [:wind-page])

(defn wind-directions []
  (let [pnames (subscribe [:all-plant-names])
        state (subscribe [:wind/wind-data])
        dates (reagent/cursor state [:dates])
        coords (subscribe [:plant-coordinates])]
    (add-watch state :wind-page/requester
       (fn [_ _ {old :dates oa :sector-angle} {new :dates na :sector-angle}]
         (when (or (not= (select-keys old [:start :end])
                         (select-keys new [:start :end]))
                   (not= oa na))
           (doseq [p @pnames]
             (remote/run-request {:url "/db/windstats"
                                  :method :get
                                  :params {:dates [(f/date-time->str (:start new))
                                                     (f/date-time->str (:end new))]
                                             :plants [p]
                                           :sector-angle na}
                                  :success [:wind/data-loaded]})))))
    (fn []
      (let [plant (:plant @state)]
       [pc/page
        [:div.container-fluid
         [:h1 (i18n [:wind-page/title]) " " (f/human-date (get-in @state [:dates :start])) " - " (f/human-date (get-in @state [:dates :end]))] 
         [:div.row         
          [:div {:class "col-sm-6"}
           [:div.shadowed.container-fluid
            [:div.row
             [date-range-picker (state/cursor dates [:dates/adjust-time-interval [:wind-page :dates]])]]
            [:div.row
             [:div.form-group
              [:label (i18n [:wind-page/angle])]
              [:input {:type "number" :default-value (:sector-angle @state)
                       :min 0
                       :max 90
                       :step 15}]
              [:button {:class "btn btn-default"
                        :on-click #(let [text (-> % .-target .-parentNode (.getElementsByTagName "input") (aget 0) .-value)
                                         num (js/parseInt text)]
                                     (when (zero? (mod 360 num))
                                       (dispatch [:wind/sector-angle text])))} (i18n [:wind-page/apply])]]]]]
          [:div {:class "col-sm-6"}
           (if (:plant @state)
             [park/wind-rose 400 plant (get-in @state [:data plant])]
             [:p (i18n [:wind-page/instructions])])]]
         [:div.shadowed
          [park/park-roses (vec @coords) @pnames (:data @state) #(dispatch [:wind/plant-selected (nth @pnames %)])]]]]))))
