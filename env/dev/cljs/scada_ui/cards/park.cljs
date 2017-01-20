(ns scada-ui.cards.park
  (:require [scada-ui.components.park :as park]
            [reagent.core :as reagent :refer [atom]]
            [reagent.interop :refer-macros [$]]
            cljsjs.d3)
  (:require-macros
     [devcards.core
      :as dc
      :refer [defcard defcard-doc defcard-rg deftest doc]]))

(def coordinates [[54.540879 6.318160] [54.510342 6.332507]
                  [54.461623 6.390108]])
(defcard-doc
  "Coordinates for this demo:"
  coordinates
  "Pixel coordinates in 100x100:" 
  (park/pixel-coords coordinates 100))

(defcard-rg plant-coordinates
  [:div {:style {:width 200}}
   [park/park-overview coordinates]])

(defcard-rg park-pie-charts
  (let [arc-fn (doto (js/d3.svg.arc)
                 ($ innerRadius 10)
                 ($ outerRadius 30))
        colors (js/d3.scale.category10)
        data [5 10 15]] 
    (park/park-renderer (fn [[x y]]
                          (doall (map-indexed
                                  (fn[idx d]
                                    ^{:key ($ d :value)}
                                    [:path {:d (arc-fn d)
                                            :fill (colors idx)
                                            :transform (str "translate(" x "," y ")")}
                                     [:title ($ d :value)]])
                                  ((js/d3.layout.pie) (clj->js data)))))
                        {:width 300
                         :height 100
                         :coordinates coordinates
                         :margin 30
                         :callback println})))

#_(defcard-rg plant-roses
  [park/park-roses coordinates (zipmap (map #(format "WEA%d" %) (range (count coordinates))) (zipmap (range 0 360 10) (repeatedly #(rand-int 20))))])
