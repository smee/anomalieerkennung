 (ns gt1-ui.tree
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [gt1-ui.util.tree :as tree]
            [reagent.core :as r]
            [reagent.core :as reagent]
            [gt1-ui.pages.common :as pc]))

(defn nop
  ([&_]))

(declare tree-nodes)

(defn tree-node
"`siblings` atom/cursor to recursivce tree nodes
`active` atom/cursor to state with keys `:selected` for all user selected nodes and `:active`for the currently selected node"
  [node
   active
   {:keys [icons mode]
    :or {icons {:leaf "glyphicon-file"
                :collapsed "glyphicon-chevron-right"
                :expanded "glyphicon-chevron-down"}}
    :as opts}]
  (let [{:keys [label children icon expanded? hidden? id]} @node
        leaf? (empty? children)]
    [:li.branch
     [:i.indicator.glyphicon
      {:class (cond
                icon icon
                leaf? (:leaf icons)
                expanded? (:expanded icons)
                :else (:collapsed icons))
       :on-click #(do 
                    (swap! node update-in [:expanded?] not)
                    (.stopPropagation %))}]
     (when (and (= mode :select) leaf?)
       [:input {:type :checkbox
                :checked (boolean (get (:selected @active) id))
                :on-click #(let [checked? (-> % .-target .-checked)]
                             (swap! active update-in [:selected] (fn [sel]
                                                                   (if checked?
                                                                     (assoc sel id @node)
                                                                     (dissoc sel id))))
                             (.preventDefault %))}])
     [:a {:class (when (= id (:id (:active @active))) "active")
          :on-click #(do 
                       (swap! active assoc :active @node)
                       (.preventDefault %))} label]
     (when (and children expanded?)
       [tree-nodes (r/cursor node [:children]) active opts])]))

(defn- tree-nodes
"`siblings` atom/cursor to recursive tree nodes
`active` atom/cursor to state with keys `:selected` for all user selected nodes and `:active`for the currently selected node"
  [siblings active opts]
  [:ul.tree
   (doall
    (for [[idx {:keys [children hidden? id] :as node}] (map-indexed vector @siblings)
          :when (not hidden?)]
      ^{:key id}
      [tree-node (reagent/cursor siblings [idx]) active opts]))])


(defn filter-text [filter]
  (r/create-class
   {:component-did-mount #(let [dom-node (r/dom-node %)
                                len (count (.-value dom-node))]
                            (.focus dom-node)
                            ; we need to manually set the cursor to the last position, per default it would be at the beginning
                            (.setSelectionRange dom-node 0 len))
    :reagent-render (fn [filter]
                      [:input.row {:placeholder "Suchbegriff"
                                   :value (:text @filter)
                                   :on-change #(let [text (-> % .-target .-value)
                                                     search-strings (str/split text " ")]
                                                 (swap! filter assoc :searches search-strings :text text))}])}))

(defn tree
  "Tree component for recursive data. `tree-data` is an atom with shape `[{:label \"lbl\", :expanded? boolean, :icon \"css-class\", :id ..., :children [...]}]`
`state` contains the tree's internal state"
  [tree-data state opts]
  (let [filter (reagent/cursor state [:filter])
        active (reagent/cursor state [:active])
        debounce (pc/debounce-factory)]
    ; update :hidden? on each tree node when filter strings change
    (add-watch filter ::update-tree
               (fn [_ _ {old :searches} {new :searches}]
                 (debounce #(when (not= old new)
                              (swap! tree-data tree/hide-non-matching new)) 300)))
    ;; initialize internal state
    (when (not @state)
      (reset! state {:show? false
                     :searches []
                     :text ""}))
    (fn [tree-data state opts]
      [:div
       [:div.row
        [:div.btn-toolbar
         [:div.btn-group
          [(if (empty? (:searches @filter))
             :button
             :button.btn-info)
           {:class "btn btn-default glyphicon glyphicon-filter"
            :title "Filter"
            :on-click #(do (swap! filter update-in [:show?] not) (.preventDefault %))}]
          [:button {:class "btn btn-default glyphicon glyphicon-resize-full"
                    :title "Auswahl ausklappen"
                    :disabled (nil? (:active @active))
                    :on-click #(swap! tree-data tree/set-expanded! (:active @active) true)}]
          [:button {:class "btn btn-default glyphicon glyphicon-resize-small"
                    :title "Auswahl einklappen"
                    :disabled (nil? (:active @active))
                    :on-click #(swap! tree-data tree/set-expanded! (:active @active) false)}]
          [:button {:class "btn btn-default glyphicon glyphicon-unchecked"
                    :title "Auswahl leeren"
                    :disabled (empty? (:selected @active))
                    :on-click #(swap! active update-in [:selected] empty)}]
          [:button {:class "btn btn-default glyphicon glyphicon-check"
                    :title "Zeige selektierte Sensoren"
                    :disabled (empty? (:selected @active))
                    :on-click #(swap! tree-data tree/expand-path-to (vals (:selected @active)))}]]]]
       (when (:show? @filter)
         [filter-text filter])
       [:div.row  ;viewport-percentage, nice
        [tree-nodes tree-data active opts]]])))
