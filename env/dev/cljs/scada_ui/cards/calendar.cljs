(ns scada-ui.cards.calendar
  (:require [reagent.core :as reagent]
            [scada-ui.components.calendar :as cal])
  (:require-macros
   [devcards.core :refer [defcard-rg deftest doc]]))

(defn date [date]
  (.setUTCHours date 0 0 0 0)
  date)

(defcard-rg availability-calendar
  (fn [data _]
    [cal/calendar @data {:extent [1 13]}])
  (let [start #inst "2015-06-01"
        end #inst "2015-06-03"
        events  [{:timestamp #inst "2015-06-01T13:55:48", :old 7, :new 1}
                 {:timestamp #inst "2015-06-01T13:58:07", :old 1, :new 13}
                 {:timestamp #inst "2015-06-01T13:59:49", :old 13, :new 3}
                 {:timestamp #inst "2015-06-01T13:59:51", :old 3, :new 4}
                 {:timestamp #inst "2015-06-01T14:00:14", :old 4, :new 10}
                 {:timestamp #inst "2015-06-01T14:00:24", :old 10, :new 11}
                 {:timestamp #inst "2015-06-01T14:12:11", :old 11, :new 10}
                 {:timestamp #inst "2015-06-01T14:12:21", :old 10, :new 5}
                 {:timestamp #inst "2015-06-01T14:13:50", :old 5, :new 7}
                 {:timestamp #inst "2015-06-01T18:58:58", :old 7, :new 14}
                 {:timestamp #inst "2015-06-01T18:58:58", :old 14, :new 1}
                 {:timestamp #inst "2015-06-01T18:59:35", :old 1, :new 13}
                 {:timestamp #inst "2015-06-02T20:56:22", :old 13, :new 3}
                 {:timestamp #inst "2015-06-02T20:56:24", :old 3, :new 4}
                 {:timestamp #inst "2015-06-02T20:56:54", :old 4, :new 10}
                 {:timestamp #inst "2015-06-02T20:57:04", :old 10, :new 5}
                 {:timestamp #inst "2015-06-02T20:59:53", :old 5, :new 7}]]    
    (reduce #(assoc % (date (:timestamp %2)) (:new %2)) {} events)))

(defcard-rg calendar
  [cal/calendar {#inst "2014-10-10" 5
                 #inst "2014-10-11" 7
                 #inst "2014-12-01" 1
                 #inst "2013-02-17" 10
                 #inst "2013-12-31" 0
                 #inst "2015-01-01" 9
                 #inst "2013-07-07" 11}])

(defcard-rg calendar
  [cal/date-range-calendar #inst "2015-04-01" #inst "2015-06-01" {:cell-size 15}])

