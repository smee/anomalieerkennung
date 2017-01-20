(ns scada-ui.components.heatmap
  (:require cljsjs.d3
            [scada-ui.remote :as remote]
            [reagent.core :as reagent]
            [reagent.format :refer [format]]
            [reagent.interop :refer [$]]))

;; source of color scales: https://github.com/politiken-journalism/scale-color-perceptual
(def viridis ["#440154" "#440256" "#450457" "#450559" "#46075a" "#46085c" "#460a5d" "#460b5e" "#470d60" "#470e61" "#471063" "#471164" "#471365" "#481467" "#481668" "#481769" "#48186a" "#481a6c" "#481b6d" "#481c6e" "#481d6f" "#481f70" "#482071" "#482173" "#482374" "#482475" "#482576" "#482677" "#482878" "#482979" "#472a7a" "#472c7a" "#472d7b" "#472e7c" "#472f7d" "#46307e" "#46327e" "#46337f" "#463480" "#453581" "#453781" "#453882" "#443983" "#443a83" "#443b84" "#433d84" "#433e85" "#423f85" "#424086" "#424186" "#414287" "#414487" "#404588" "#404688" "#3f4788" "#3f4889" "#3e4989" "#3e4a89" "#3e4c8a" "#3d4d8a" "#3d4e8a" "#3c4f8a" "#3c508b" "#3b518b" "#3b528b" "#3a538b" "#3a548c" "#39558c" "#39568c" "#38588c" "#38598c" "#375a8c" "#375b8d" "#365c8d" "#365d8d" "#355e8d" "#355f8d" "#34608d" "#34618d" "#33628d" "#33638d" "#32648e" "#32658e" "#31668e" "#31678e" "#31688e" "#30698e" "#306a8e" "#2f6b8e" "#2f6c8e" "#2e6d8e" "#2e6e8e" "#2e6f8e" "#2d708e" "#2d718e" "#2c718e" "#2c728e" "#2c738e" "#2b748e" "#2b758e" "#2a768e" "#2a778e" "#2a788e" "#29798e" "#297a8e" "#297b8e" "#287c8e" "#287d8e" "#277e8e" "#277f8e" "#27808e" "#26818e" "#26828e" "#26828e" "#25838e" "#25848e" "#25858e" "#24868e" "#24878e" "#23888e" "#23898e" "#238a8d" "#228b8d" "#228c8d" "#228d8d" "#218e8d" "#218f8d" "#21908d" "#21918c" "#20928c" "#20928c" "#20938c" "#1f948c" "#1f958b" "#1f968b" "#1f978b" "#1f988b" "#1f998a" "#1f9a8a" "#1e9b8a" "#1e9c89" "#1e9d89" "#1f9e89" "#1f9f88" "#1fa088" "#1fa188" "#1fa187" "#1fa287" "#20a386" "#20a486" "#21a585" "#21a685" "#22a785" "#22a884" "#23a983" "#24aa83" "#25ab82" "#25ac82" "#26ad81" "#27ad81" "#28ae80" "#29af7f" "#2ab07f" "#2cb17e" "#2db27d" "#2eb37c" "#2fb47c" "#31b57b" "#32b67a" "#34b679" "#35b779" "#37b878" "#38b977" "#3aba76" "#3bbb75" "#3dbc74" "#3fbc73" "#40bd72" "#42be71" "#44bf70" "#46c06f" "#48c16e" "#4ac16d" "#4cc26c" "#4ec36b" "#50c46a" "#52c569" "#54c568" "#56c667" "#58c765" "#5ac864" "#5cc863" "#5ec962" "#60ca60" "#63cb5f" "#65cb5e" "#67cc5c" "#69cd5b" "#6ccd5a" "#6ece58" "#70cf57" "#73d056" "#75d054" "#77d153" "#7ad151" "#7cd250" "#7fd34e" "#81d34d" "#84d44b" "#86d549" "#89d548" "#8bd646" "#8ed645" "#90d743" "#93d741" "#95d840" "#98d83e" "#9bd93c" "#9dd93b" "#a0da39" "#a2da37" "#a5db36" "#a8db34" "#aadc32" "#addc30" "#b0dd2f" "#b2dd2d" "#b5de2b" "#b8de29" "#bade28" "#bddf26" "#c0df25" "#c2df23" "#c5e021" "#c8e020" "#cae11f" "#cde11d" "#d0e11c" "#d2e21b" "#d5e21a" "#d8e219" "#dae319" "#dde318" "#dfe318" "#e2e418" "#e5e419" "#e7e419" "#eae51a" "#ece51b" "#efe51c" "#f1e51d" "#f4e61e" "#f6e620" "#f8e621" "#fbe723" "#fde725"])

(def inferno ["#000004" "#010005" "#010106" "#010108" "#02010a" "#02020c" "#02020e" "#030210" "#040312" "#040314" "#050417" "#060419" "#07051b" "#08051d" "#09061f" "#0a0722" "#0b0724" "#0c0826" "#0d0829" "#0e092b" "#10092d" "#110a30" "#120a32" "#140b34" "#150b37" "#160b39" "#180c3c" "#190c3e" "#1b0c41" "#1c0c43" "#1e0c45" "#1f0c48" "#210c4a" "#230c4c" "#240c4f" "#260c51" "#280b53" "#290b55" "#2b0b57" "#2d0b59" "#2f0a5b" "#310a5c" "#320a5e" "#340a5f" "#360961" "#380962" "#390963" "#3b0964" "#3d0965" "#3e0966" "#400a67" "#420a68" "#440a68" "#450a69" "#470b6a" "#490b6a" "#4a0c6b" "#4c0c6b" "#4d0d6c" "#4f0d6c" "#510e6c" "#520e6d" "#540f6d" "#550f6d" "#57106e" "#59106e" "#5a116e" "#5c126e" "#5d126e" "#5f136e" "#61136e" "#62146e" "#64156e" "#65156e" "#67166e" "#69166e" "#6a176e" "#6c186e" "#6d186e" "#6f196e" "#71196e" "#721a6e" "#741a6e" "#751b6e" "#771c6d" "#781c6d" "#7a1d6d" "#7c1d6d" "#7d1e6d" "#7f1e6c" "#801f6c" "#82206c" "#84206b" "#85216b" "#87216b" "#88226a" "#8a226a" "#8c2369" "#8d2369" "#8f2469" "#902568" "#922568" "#932667" "#952667" "#972766" "#982766" "#9a2865" "#9b2964" "#9d2964" "#9f2a63" "#a02a63" "#a22b62" "#a32c61" "#a52c60" "#a62d60" "#a82e5f" "#a92e5e" "#ab2f5e" "#ad305d" "#ae305c" "#b0315b" "#b1325a" "#b3325a" "#b43359" "#b63458" "#b73557" "#b93556" "#ba3655" "#bc3754" "#bd3853" "#bf3952" "#c03a51" "#c13a50" "#c33b4f" "#c43c4e" "#c63d4d" "#c73e4c" "#c83f4b" "#ca404a" "#cb4149" "#cc4248" "#ce4347" "#cf4446" "#d04545" "#d24644" "#d34743" "#d44842" "#d54a41" "#d74b3f" "#d84c3e" "#d94d3d" "#da4e3c" "#db503b" "#dd513a" "#de5238" "#df5337" "#e05536" "#e15635" "#e25734" "#e35933" "#e45a31" "#e55c30" "#e65d2f" "#e75e2e" "#e8602d" "#e9612b" "#ea632a" "#eb6429" "#eb6628" "#ec6726" "#ed6925" "#ee6a24" "#ef6c23" "#ef6e21" "#f06f20" "#f1711f" "#f1731d" "#f2741c" "#f3761b" "#f37819" "#f47918" "#f57b17" "#f57d15" "#f67e14" "#f68013" "#f78212" "#f78410" "#f8850f" "#f8870e" "#f8890c" "#f98b0b" "#f98c0a" "#f98e09" "#fa9008" "#fa9207" "#fa9407" "#fb9606" "#fb9706" "#fb9906" "#fb9b06" "#fb9d07" "#fc9f07" "#fca108" "#fca309" "#fca50a" "#fca60c" "#fca80d" "#fcaa0f" "#fcac11" "#fcae12" "#fcb014" "#fcb216" "#fcb418" "#fbb61a" "#fbb81d" "#fbba1f" "#fbbc21" "#fbbe23" "#fac026" "#fac228" "#fac42a" "#fac62d" "#f9c72f" "#f9c932" "#f9cb35" "#f8cd37" "#f8cf3a" "#f7d13d" "#f7d340" "#f6d543" "#f6d746" "#f5d949" "#f5db4c" "#f4dd4f" "#f4df53" "#f4e156" "#f3e35a" "#f3e55d" "#f2e661" "#f2e865" "#f2ea69" "#f1ec6d" "#f1ed71" "#f1ef75" "#f1f179" "#f2f27d" "#f2f482" "#f3f586" "#f3f68a" "#f4f88e" "#f5f992" "#f6fa96" "#f8fb9a" "#f9fc9d" "#fafda1" "#fcffa4"])

(defn log [x]
  (if (zero? x)
    x
    (let [sign (if (neg? x) -1 1)]
      (* sign (js/Math.log x)))))

(defn y-axis-width [{:keys [axis? y-label]}]
  (cond
    (and axis? y-label) 60
    axis? 40
    :else 0))

(defn x-axis-height [{:keys [axis? x-label]}]
  (cond
    (and axis? x-label) 30
    axis? 20
    :else 0))

(defn heatmap
"Canvas based heatmap visualization. `data` is [[double]] of n rows of m columns each.
`extents` is `{:keys [min-x max-x min-y max-y]}`, needed for axis rendering.
opts is `{:keys [width height colors grey? inverse? log? axis? ticks]}`"
  [data extents opts]
  (let [find-canvas #(aget (.getElementsByTagName (reagent/dom-node %) "canvas") 0)
        render (fn [canvas data {:keys [width height colors grey? inverse? log? x-label y-label]
                                :or {colors viridis} :as opts}]
                 (let [width (.-width canvas)
                       height (.-height canvas)
                       rows (count data)
                       cols (count (first data))
                       dy (js/Math.floor (/ height rows))
                       dx (js/Math.floor (/ width cols))
                       [mi ma] (apply (juxt min max) (reduce into [] data))
                       mi (if log? (log mi) mi)
                       ma (if log? (log ma) ma)
                       colors (cond->> colors
                                grey? (mapv #(let [c (js/d3.hcl %)] (aset c "c" 0) c) colors)
                                inverse? (reverse))
                       color-scale (doto (js/d3.scaleQuantize)
                                     ($ domain #js [mi ma])
                                     ($ range (clj->js colors)))
                       ctx (.getContext canvas "2d")] 
                   (doseq [[i row] (map-indexed vector data)
                           [j d] (map-indexed vector row)]
                     (aset ctx "fillStyle" (color-scale (if log? (log d) d)))
                     (.fillRect ctx (* j dx) (* (dec (- rows i)) dy) dx dy))))
        adjust-dim (fn [opts]
                     (-> opts
                         (update :width - (y-axis-width opts))
                         (update :height - (x-axis-height opts))))]
    (reagent/create-class
     {:component-did-mount (fn [this]
                             (render (find-canvas this) data (adjust-dim opts)))
      :component-will-update (fn [this [_ data _ opts]]
                               ;; the general flow is :component-will-update (with new arguments) -> :reagent-render (with new arguments) -> :component-did-update (with PREVIOUS arguments)
                               ;; since we need to redraw the canvas after recreating it in :reagent-render, we need to wait a few milliseconds to give react time to recreate the canvas
                               ;; after resizing
                               (js/setTimeout (fn [] (render (find-canvas this) data (adjust-dim opts))) 100))
      :reagent-render (fn [_ {:keys [min-x max-x min-y max-y]}
                          {:keys [width height axis? ticks x-label y-label]
                           :or {width 64 height 48 ticks 4} :as opts}]
                        (let [y-axis-w (y-axis-width opts)
                              x-axis-h (x-axis-height opts)
                              canvas-w (- width y-axis-w)
                              canvas-h (- height x-axis-h)
                              tick-x (/ (- max-x min-x) ticks)
                              tick-y (/ (- max-y min-y) ticks)
                              x-scale (doto (js/d3.scaleLinear) ($ domain #js [min-x max-x]) ($ range #js [0 canvas-w]))
                              y-scale (doto (js/d3.scaleLinear) ($ domain #js [min-y max-y]) ($ range #js [canvas-h 0]))]
                          [:div {:style {:position "relative"}}
                           (when axis?                             
                             [:svg {:width width :height height}
                              [:g {:transform (format "translate(%f,-20)" y-axis-w)}
                               (when y-label
                                 [:g {:transform (format "translate(%f,%f)rotate(-90)" (- 10 y-axis-w) (/ height 2))} 
                                  [:text {:text-anchor "middle"} y-label]])
                               (when x-label
                                 [:g {:transform (format "translate(%f,%f)" (/ width 2) height)} 
                                  [:text {:text-anchor "middle"} x-label]])
                               [:g {:class "x axis" :transform (format "translate(0,%f)" (- height x-axis-h))}
                                (for [x (range ticks) :let [x (+ min-x (* x tick-x))]] 
                                  ^{:key x}
                                  [:g.tick {:transform (format "translate(%f,0)" (x-scale x))}
                                   [:line {:y2 6 :x1 0.5 :x2 0.5 :stroke "black"}]
                                   [:text {:dy ".71em" :y 9 :x 0.5}
                                    (format "%.2f" x)]])]
                               [:g {:class "y axis" :text-anchor :end}
                                (for [y (range ticks) :let [y (+ min-y (* y tick-y))]]
                                  ^{:key y}
                                  [:g.tick {:transform (format "translate(0,%f)" (y-scale y))}
                                   [:line {:x2 -6 :y1 0.5 :y2 0.5 :stroke "black"}]
                                   [:text {:dy ".32em" :x -9 :y 0.5} (format "%.2f" y)]])]]])
                           [:canvas {:width canvas-w
                                     :height canvas-h
                                     :style {:position "absolute"
                                             :left y-axis-w
                                             :top 0}}]]))})))
