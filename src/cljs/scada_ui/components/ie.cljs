(ns scada-ui.components.ie)

(defn svg-scaling-hack
"Internet Explorer insists on 150px height for svg. It does not scale to the available space (even with width:100%).
Workaround: Canvases get scaled, so we render a canvas with the correct aspect ratio, hide it and render the svg on top.
For details refer to http://nicolasgallagher.com/canvas-fix-svg-scaling-in-internet-explorer/"
  [width height [kw-svg opts body :as svg]]
  {:pre [(= :svg kw-svg) (map? opts)]}
  [:div {:style {:width "100%" :display "inline-block" :position :relative}}
   [:canvas {:width width :height height
             :style {:display "block"
                     :width "100%"
                     :height "100% !important"
                     :visibility "hidden"}}]
   [:svg (assoc opts :style {:width "100%" :position :absolute :top 0 :left 0})
    body]])
