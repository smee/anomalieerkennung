(ns scada-ui.components.park
  (:require [reagent.format :refer [format]]
            [reagent.core :as reagent]
            [re-frame.core :refer [subscribe dispatch]]
            [reagent.interop :refer [$]]
            [clojure.string :as str]
            [scada-ui.i18n :refer [i18n]]
            [scada-ui.components.bootstrap :as bs]
            [scada-ui.components.ie :as ie])
  (:import goog.math.interpolator.Linear1))

(defn wind-rose
"Render a wind rose visualization. `size` is in pixel, `text` gets printed in the middle if `size` is big enough.
`sector-values` is a map of directions to {:n relative weight, :v value}."
  [size text sector-values]
  (let [mx (/ size 2)
        my (/ size 2)
        tiny? (<= size 100)
        padding (if tiny? 0 30)
        speeds [5 10 15]
        speed->radius (doto (Linear1.)
                        (.setData #js [0 (last speeds)] #js [padding (* 0.5 (- size padding))]))
        inner-radius (.interpolate speed->radius 0)
        [maximum sum] (apply (juxt max +) (map :n (vals sector-values)))]
    [:svg.wind-rose {:width size :height size}
     ;; speed circles
     [:g.axes (map-indexed (fn [idx speed]
                             ^{:key idx} [:circle {:cx "50%"
                                                   :cy "50%"
                                                   :fill "transparent" ;; without explicit fill clicking within the boundaries won't have this circle as target
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
        (for [dir (range 30 360 30)]
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
             arc-fn (doto (js/d3.arc)
                        ($ innerRadius inner-radius)
                        ($ startAngle #(* (/ js/Math.PI 180) (- (first %) half-angle)))
                        ($ endAngle #(* (/ js/Math.PI 180) (+ (first %) half-angle)))
                        ($ outerRadius #($ speed->radius interpolate (second %))))
             colors (doto (js/d3.scaleLinear)
                        ($ domain #js [0 maximum])
                        ($ range #js ["white" "darkblue"]))]
         [:g.arcs
          (doall
           (for [[angle {speed :v n :n}] sector-values]
             ^{:key angle}
             [:path {:d (arc-fn [angle speed])
                     :transform (format "translate(%f,%f)" mx my)
                     :fill (colors n)}
              [:title (i18n [:wind-rose-tooltip] [(format "%.1f" (* 100 (/ n sum))) angle (format "%.1f" speed)])]]))]))]))


(defn pixel-coords
"Convert geographical coordinates into pixel coordinates of a specified `width`. 
This function
will return the real `{:keys [width height]} as meta data`."
  [coords width]
  (let [path ($ (js/d3.geoPath) projection (doto (js/d3.geoMercator)
                     ($ translate #js [0 0])
                     ($ scale 1)))
        [[left bottom] [right top]] ($ path bounds (clj->js {:type "MultiPoint" :coordinates (mapv reverse coords)}))
        dx (- right left)
        dy (- top bottom)
        ratio (if (or (zero? dx) (zero? dy)) 1 (/ dy dx))
        height (* ratio width)
        scale (/ 1.0 (max (/ dx width)
                           (/ dy height)))
        trans [(* 0.5 (- width (* scale (+ left right))))
               (* 0.5 (- height (* scale (+ top bottom))))]
        projection (doto (js/d3.geoMercator)
                     ($ translate (clj->js trans))
                     ($ scale scale))]
    (with-meta (mapv #(projection (clj->js (reverse %))) coords) {:width width :height height})))



(defn park-renderer
  [plant-creator {:keys [callback width height coordinates margin]
                  :or {width 450
                       coordinates []
                       margin 0}}] ;; TODO add option to fill svg horizontally. needs to know width, width per element, max number of elements horizontally
  (let [[left top right bottom] (if (sequential? margin) margin [margin margin margin margin])
        coords (pixel-coords coordinates width)
        {:keys [width height]} (meta coords)
        total-h (+ height top bottom)
        total-w (+ width left right)]
    (ie/svg-scaling-hack total-w total-h
      [:svg {:viewBox (str/join " " [(- left) (- top) total-w total-h])
             :preserveAspectRatio "xMinYMin meet"             
             :on-click (when callback ;; add one callback for the whole svg
                         #(let [target (.-target %)]
                            (loop [elem target]
                              (let [parent (.-parentNode elem)]
                                (cond
                                  (nil? parent) nil
                                  (.hasAttribute elem "data-index") (callback (js/parseInt (.getAttribute elem "data-index")))
                                  :else (recur parent))))))}
       (doall
        (for [[idx [x y :as coords]] (map list (range) coords)]
          ^{:key (str coords)}
          [:g.plant {:data-index idx
                     :class (when callback "clickable")}
           (plant-creator [x y] idx)]))])))

(defn park-overview [coords]
  (park-renderer (fn [[x y]]
                   [:circle {:cx x :cy y :r 3 :fill "black"}])
                 {:callback println
                  :coordinates coords
                  :width 200
                  :margin 10}))


(defn park-roses [coords names values callback]
  (let [diameter 75
        dx (* diameter 0.5)
        dy dx
        idx->name (into {} (map-indexed (comp vec reverse list) names))]
    (park-renderer (fn [[x y] idx]
                     (let [name (nth names idx)]
                       [:g {:transform (format "translate(%f,%f)" (- x dx) (- y dy))} ;; center wind rose at [x y] coordinate
                        [wind-rose diameter name (values name)]]))
                   {:coordinates coords
                    :callback (or callback (fn []))
                    :width 700
                    :height 700
                    :margin dx})))


(defn plant-selector []
  (let [coords (subscribe [:plant-coordinates])
        plants (subscribe [:plants])
        selected (subscribe [:selected-plants])]
    (fn []
      (let [radius 8
            n (count @selected)]
        (bs/popup {}
                  [:button {:class "btn btn-default"
                            :title (i18n ["Auswahl der zu visualisierenden Anlagen"])}
                   [:img {:src "images/wea-icon.svg"}]
                   (when (not= 0 n)
                     [:span {:style {:color "green"}} (format "(%d)" n)])
                   [:i.caret]]
                  [:div {:style {:width 600}}
                   [park-renderer (fn [[x y] idx]
                                    (let [{:keys [name selected?]} (nth @plants idx)]
                                      [:g {:transform (format "translate(%f,%f)" radius radius)}
                                       [:circle {:cx x :cy y :r radius
                                                 :fill (if selected? "blue" "white")
                                                 :stroke "black"}]
                                       [:text {:transform (format "translate(%f,%f)" (- x 8) (+ y 20))} name]]))
                    {:width 600
                     :height 600
                     :margin [20 20 40 40]
                     :coordinates @coords
                     :callback (fn [idx] (dispatch [:toggle-plant idx]))}]
                   [:button {:class "btn btn-primary"
                             :on-click #(dispatch [:select-all-plants false])} (i18n [:none "Keine"])]
                   [:button {:class "btn btn-danger"
                             :title (i18n ["Das Laden von Daten kann entsprechend lange dauern!"])
                             :on-click #(dispatch [:select-all-plants true])} (i18n [:all "Alle"])]])))))
