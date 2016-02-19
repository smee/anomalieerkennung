(ns gt1-ui.matrix
  (:require [clojure.set :as set]
            [reagent.format :refer [format]]
            [reagent.interop :refer-macros [$]]))
(defn name-of [node]
  (cond
    (string? node) node
    (map? node) (:name node)
    :else (throw (ex-info "invalid node type, can't extract name!" {:node node}))))

(defn reachable-nodes [adj start-node]
  (loop [queue #{(name-of start-node)}, seen #{}]
    (if (empty? queue)
      seen
      (let [node (first queue)
            queue (into queue (map name-of (adj node)))]
        (recur (set/difference queue seen)
               (set/union queue seen))))))

(defn reverse-span-tree [adj target]  
  (let [nodes (reachable-nodes adj target)]
    [(sort (filter nodes (keys adj)))
     (into {}
           (for [[k vs] adj
                 :when (nodes k)]
             [k (filter (comp nodes name-of) vs)]))]))

(defn matrix [row-names adjacencies {:keys [cell-size background?]
                                     :or {cell-size 7
                                          background? true}}]
  (let [weighted? (every? map? (first (vals adjacencies)))
        col-names (->> adjacencies vals (reduce #(into %1 (map name-of %2)) []) distinct sort)
        width (* cell-size (count col-names))
        height (* cell-size (count row-names))
        margin 80
        row-idx (zipmap row-names (range))
        col-idx (zipmap col-names (range))
        y (-> ($ js/d3 scale.ordinal)
              ($ domain (clj->js (range (count row-names))))
              ($ rangeBands #js [0 height]))
        x (-> ($ js/d3 scale.ordinal)
              ($ domain (clj->js (range (count col-names))))
              ($ rangeBands #js [0 width]))
        c-col ($ x rangeBand)
        c-row ($ y rangeBand)]
    [:div {:style {:overflow-x "scroll"
                   :height (+ height margin margin)}}
     [:svg.matrix {:width (+ margin width)
                   :height (+ margin height)}
      [:g {:transform (format "translate(%f,%f)" margin margin)}
       (when background?
         [:rect.background {:width width
                            :height height}])
       ;; rows
       (doall
        (for [[n i] row-idx
              :let [row (get adjacencies n)]]
          ^{:key n}
          [:g.row {:transform (format "translate (0,%f)" (y i))}
           [:line {:x2 width}]
           [:text {:x -6
                   :y (/ c-row 2)
                   :dy "0.32em"
                   :text-anchor "end"
                   :font-size cell-size}
            (str n)]
           (doall
            (for [cell row]
              ^{:key (name-of cell)}
              [:rect.cell {:x (x (col-idx (name-of cell)))
                           :width c-row
                           :height c-row
                           :fill "blue"
                           :opacity (when weighted? (:weight cell))}
               (when weighted?
                 [:title (format "%.4f" (:weight cell))])]))]))
       ;; columns
       (doall
        (for [[n i] col-idx]
          ^{:key i}
          [:g.column {:transform (format "translate(%f)rotate(-90)" (x i))}
           [:line {:x1 (- height)}]
           [:text {:x 6
                   :y (/ c-col 2)
                   :dy "0.32em"
                   :text-anchor "start"
                   :font-size cell-size}
            (str n)]]))]]]))


(defn directed-graph [adjacencies]
  (let [width 960
        height 500
        nodes (vec (into (set (keys adjacencies))
                         (->> adjacencies vals (reduce #(into %1 (map name-of %2)) []))))
        node->idx (into {} (map-indexed #(vector %2 %1) nodes))
        links (for [[k targets] adjacencies, target targets]
                {:source (node->idx (name-of k))
                 :target (node->idx (name-of target))                 
                 :weight (if (map? target) (:weight target) 1)})
        force (-> ($ js/d3 layout.force)
                  ($ nodes (clj->js (map #(hash-map :name %) nodes)))
                  ($ links (clj->js links))
                  ($ size #js [width height])
                  ($ linkDistance 150)
                  ($ linkDistance (fn [link] (- 120 (* (.-weight link) 100)))) ;; try to get shorter edges for bigger weights
                  ($ charge -300)
                  ($ start))]
    (dotimes [_ 400] ($ force tick))
    [:svg {:width width :height height :class "directed-graph"}
     [:defs
      [:marker {:id "end"
                :viewBox "0 -5 10 10"
                :refX 15 ;; FIXME unsupported by react
                :refY -1.5  ;; FIXME unsupported by react
                :markerWidth 6  ;; FIXME unsupported by react
                :markerHeight 6  ;; FIXME unsupported by react
                :orient "auto" ;; FIXME unsupported by react
                }
       [:path {:d "M0,-5L10,0L0,5"}]]]
     ;; nodes
     (doall (for [{:strs [name x y]} (js->clj ($ force nodes))]
        ^{:key name}
        [:g.node {:transform (format "translate(%f,%f)" x y)}
         [:circle {:r 5}]
         [:text name]]))
     ;; edges
     (doall (for [{{sx "x" sy "y" sn "name"} "source" {tx "x" ty "y" tn "name"} "target" w "weight"} (js->clj ($ force links))
                  :let [dx (- tx sx)
                        dy (- ty sy)
                        dr (.sqrt js/Math (+ (* dx dx) (* dy dy)))]]
              ^{:key (str sn "-" tn)}
              [:path.link {:marker-end "url(#end)"
                           :opacity (when w w)
                           :d (format "M%f,%fA%f,%f 0 0,1 %f,%f" sx sy dr dr tx ty)}]))
     ])
  )
