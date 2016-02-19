(ns gt1-ui.pages.wind-page
  (:require [gt1-ui.park :as park]
            [gt1-ui.pages.common :as pc]
            [gt1-ui.remote :as remote]
            [gt1-ui.pages.common :refer [date-time->str]]
            [reagent.core :as reagent]
            [reagent.session :as session]
            [cljs-time.core :as time]))


(defn wind-directions []
  (let [plants (session/get :plants)
        pnames (mapv :name plants)
        data (reagent/atom (vec (repeat (count plants) {})))]
    (remote/GET "/db/windstats" {:params {:dates [(date-time->str (time/date-time 2016 2 22))
                                                  (date-time->str (time/date-time 2016 2 23))]
                                          :plants pnames}
                                 :handler #(reset! data (mapv % pnames))})
    (fn []
      [park/park-roses (vec @pc/coords) @data])))
