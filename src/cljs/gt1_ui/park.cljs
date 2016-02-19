(ns gt1-ui.park
  (:require [reagent.format :refer [format]]
            [reagent.interop :refer-macros [$]])
  (:import goog.math.interpolator.Linear1))

(defn wind-rose
"Render a wind rose visualization. `size` is in pixel, `text` gets printed in the middle if `size` is big enough.
`sector-values` is a map of directions to {:n relative weight, :v value}."
  [size text sector-values]
  (let [mx (/ size 2)
        my (/ size 2)
        tiny? (<= size 100)
        padding (if tiny? 0 30)
        speeds [5 10 15 20]
        speed->radius (doto (Linear1.)
                        (.setData #js [0 20] #js [padding (* 0.5 (- size padding))]))
        inner-radius (.interpolate speed->radius 0)
        maximum (apply max (map :n (vals sector-values)))]
    [:svg.wind-rose {:width size :height size}
     ;; speed circles
     [:g.axes (map-indexed (fn [idx speed]
                             ^{:key idx} [:circle {:cx "50%"
                                                   :cy "50%"
                                                   :data-speed speed
                                                   ;:r (* (inc idx) per-ring)
                                                   :r (.interpolate speed->radius speed)}])
                           speeds)]
     (when (not tiny?)
       ;; labels for every 2nd speed circle
      [:g.tickmarks
       (for [speed (take-nth 2 speeds)]
         ^{:key speed} [:text {:dy "-2px"
                               :transform (format "translate(%f,%f)" mx (- mx (.interpolate speed->radius speed)))}
                        (str speed "m/s")])])
     ;; wind directions around the outermost circle
     (when (not tiny?)
       [:g.labels
        (for [dir (range 30 361 30)]
          ^{:key dir}
          [:text {:dy "-4px"
                  :transform (format "translate(%f,%f) rotate(%f,%f,%f)"
                                     mx (* 0.5 padding)
                                     dir 0 (- mx (* 0.5 padding)))}
           (str dir "°")])])
     (when (not tiny?)
       ;; text in the middle
       [:g.central
        [:text {:x "50%" :y "50%"} text]])
     ;; if we have a map of directions to wind speeds, create colored arcs
     (when sector-values
       (let [half-angle (* 0.5 (/ 360 (count sector-values)))
             arc-fn (doto (js/d3.svg.arc)
                        ($ innerRadius inner-radius)
                        ($ startAngle #(* (/ js/Math.PI 180) (- (first %) half-angle)))
                        ($ endAngle #(* (/ js/Math.PI 180) (+ (first %) half-angle)))
                        ($ outerRadius #($ speed->radius interpolate (second %))))
             colors (doto (js/d3.scale.linear)
                        ($ domain #js [0,maximum])
                        ($ range #js ["white" "darkblue"]))]
         [:g.arcs
          (for [[angle {speed :v n :n}] sector-values]
            ^{:key angle}
            [:path {:d (arc-fn [angle speed])
                    :transform (format "translate(%f,%f)" mx my)
                    :fill (colors n)}
             [:title (format "%f° %.1fm/s" angle speed)]])]))]))



#_(defn pixel-coords
"Convert geographical coordinates into pixel coordinates of a specified `width` and `height`."
  [coords width height]
  (let [xmi (apply min (map second coords))
        xma (apply max (map second coords))
        ymi (apply min (map first coords))
        yma (apply max (map first coords))
        dx (- xma xmi)
        dy (- yma ymi)
        center #js [(/ (+ xma xmi) 2) (/ (+ yma ymi) 2)]        
        projection (doto (js/d3.geo.mercator)
                     ($ translate #js [(/ width 2) (/ height 2)])
                     ($ scale (/ 30 (min (/ dx width) (/ dy height))))
                     ($ center center))]
    (mapv #(projection (clj->js (reverse %))) coords)))

(defn pixel-coords
"Convert geographical coordinates into pixel coordinates of a specified `width` and `height`."
  [coords width height]
  (let [projection (doto (js/d3.geo.mercator)
                     ($ translate #js [0 0])
                     ($ scale 1))
        path ($ (js/d3.geo.path) projection projection)
        [[left bottom] [right top]] ($ path bounds (clj->js {:type "MultiPoint" :coordinates (mapv reverse coords)}))
        dx (- right left)
        dy (- top bottom)
        ratio (/ dy dx)
        height (* ratio height)
        scale (/ 0.9 (max (/ dx width)
                           (/ dy height)))
        trans [(* 0.5 (- width (* scale (+ left right))))
               (* 0.5 (- height (* scale (+ top bottom))))]
        projection (doto (js/d3.geo.mercator)
                     ($ translate (clj->js trans))
                     ($ scale scale))]
    (with-meta (mapv #(projection (clj->js (reverse %))) coords) {:width width :height height})))

(defn park-renderer
  [plant-creator {:keys [callback width height coordinates]
                  :or {width 450
                       height 600
                       coordinates []}}]
  (let [coords (pixel-coords coordinates width height)
        {:keys [width height]} (meta coords)]
    [:svg {:width width
           :height height
           :on-click (when callback ;; add one callback for the whole calendar
                       #(let [target (.-target %)]
                         (loop [elem target]
                           (let [parent (.-parentNode elem)]
                             (cond
                               (nil? parent) nil
                               (.hasAttribute elem "data-index") (callback (js/parseInt (.getAttribute elem "data-index")))
                               :else (recur parent))))))}
     (doall
      (for [[idx coords] (map list (range) coords)]       
        ^{:key (str coords)}
        [:g.plant {:data-index idx
                   :class (when callback "clickable")}
         (plant-creator coords idx)]))]))

(defn park-overview [coords]
  (park-renderer (fn [[x y]]
                   [:circle {:cx x :cy y :r 3 :fill "black"}])
                 {:callback println
                  :coordinates coords
                  :width 400
                  :height 600}))


(defn park-roses [coords values]
  (park-renderer (fn [[x y] idx]
                   [:g {:transform (format "translate(%f,%f)" x y)}
                    [wind-rose 50 "" (nth values idx)]])
                 {:coordinates coords
                  :width 700
                  :height 700}))
