 (ns scada-ui.components.tree
  (:require [clojure.string :as str]
            [scada-ui.i18n :refer [i18n]]
            [scada-ui.util.tree :as tree]
            [scada-ui.components.park :as park]
            [reagent.core :as r]
            [re-frame.core :refer [reg-event-db reg-event-fx path reg-sub dispatch subscribe]]))

;;;;;;;;;; subscriptions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-sub :tree/state (fn [db] (select-keys (:tree db) [:selected? :active :hidden? :expanded?])))
(reg-sub :tree/filter (fn [db] (-> db :tree :filter)))
(reg-sub :tree/filter-text :<- [:tree/filter](fn [{text :text}] (str/split text " ")))
(reg-sub :tree/nodes :<- [:tree] (fn [tree] (reverse (tree/nodes tree))))
(reg-sub :tree/hidden?
         :<- [:tree/filter-text]
         :<- [:tree/nodes]
         (fn [[searches nodes]]
           (reduce (fn [hidden? {:keys [label id children]}]
                     (let [matches? (every? #(js/goog.string.caseInsensitiveContains label %) searches)
                           leaf? (empty? children)
                           children-hidden? (every? (comp hidden? :id) children)]
                       (if (or (and (not matches?) leaf?)
                               (and (not leaf?) children-hidden?))
                         (conj hidden? id)
                         hidden?))) #{} nodes)))
(reg-sub :tree/selected-ids :<- [:tree/state] (fn [state] (:selected? state)))

(reg-sub :tree/active-node
         :<- [:tree/state]
         :<- [:id->node]
         (fn [[state id->node]]
           (id->node (:active state))))

;;;;;;;;;;;;;;; handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(reg-event-db :tree/toggle (path :tree)
   (fn [db [_ set-name id]]
     (update-in db [set-name] 
                (fnil (fn [set] 
                       (if (contains? set id)
                         (disj set id)
                         (conj set id))) #{}))))

(reg-event-db :tree/select-all (path :tree)
                 (fn [tree [_ ids]]
                   (assoc tree :selected? (set ids))))

(reg-event-db :tree/set-active (path :tree)
  (fn [tree [_ id]]
    (assoc tree :active id)))

(reg-event-db :tree/focus-selected (path :tree)
  (fn [{:keys [selected?] :as db} [_]]
    (assoc db :expanded? (tree/ids-on-path @(subscribe [:tree]) selected?))))

(reg-event-db :tree/clear-selection (path :tree)
   (fn [tree [_ ]] (assoc tree :selected? #{})))

(reg-event-db :tree/set-filter (path :tree)
   (fn [tree [_ text]] (assoc-in tree [:filter :text] text)))


(reg-event-db :tree/toggle-filter (path :tree)
   (fn [tree] (update-in tree [:filter :show?] not)))

(reg-event-db :tree/expand-collapse-subtree (path :tree)
   (fn [tree [_ direction]]
     (let [f (case direction :expand into :collapse (fn [s ids] (apply disj s ids)))]
       (if-let [node (tree/find-by-id @(subscribe [:tree]) (:active tree))]
         (update-in tree [:expanded?] f (map :id (tree/nodes node)))
         tree))))

;;;;;;;;;;; tree UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare tree-nodes)

(defn tree-node
""
  [node {:keys [expanded? selected? active] :as state}
   hidden?
   {:keys [icons mode]
    :or {icons {:leaf "glyphicon-file"
                :collapsed "glyphicon-chevron-right"
                :expanded "glyphicon-chevron-down"}}
    :as opts}]
  (let [{:keys [label children icon id]} node
        leaf? (nil? children)
        expanded? (expanded? id)]
    [:li.branch
     [:i.indicator.glyphicon
      {:class (cond
                icon icon
                leaf? (:leaf icons)
                expanded? (:expanded icons)
                :else (:collapsed icons))
       :on-click #(dispatch [:tree/toggle :expanded? id])}]
     (when (and (= mode :select) (or leaf? (= :sensor (:type node)))) ;; FIXME type is application specific, meaning this tree component is not generic anymore!
       [:input {:type :checkbox
                :checked (selected? id)
                :on-change #(dispatch [:tree/toggle :selected? id])}])
     [:span {:class (when (= id active) "active")
             :on-click #(do
                          (dispatch [:tree/set-active id])
                          (when (not expanded?) ;; expand if currently collapsed
                            (dispatch [:tree/toggle :expanded? id])))}
      (i18n [(keyword :tree label) label])]
     (when (:button node)
       [:button.btn.btn-default.btn-xs {:on-click #(dispatch (-> node :button :dispatch-v))} (-> node :button :contents)])
     (when (and children expanded?)
       [tree-nodes children state hidden? opts])]))

(defn- tree-nodes
""
  [siblings state hidden? opts]
  [:ul.tree
   (doall
    (for [{:keys [children id] :as node} siblings
          :when (not (hidden? id))]
      ^{:key id}
      [tree-node node state hidden? opts]))])

(defn- filter-changed [event]
  (let [text (-> event .-target .-value)]
    (dispatch [:tree/set-filter text])))

(defn filter-text [filter]
  (r/create-class
   {:component-did-mount (fn [this]
                           (let [dom-node (r/dom-node this)
                                 len (count (.-value dom-node))]
                             (.focus dom-node)
                             ;; we need to manually set the cursor to the last position, per default it would be at the beginning
                             (.setSelectionRange dom-node 0 len)))
    :reagent-render (fn [filter]
                      [:input.row {:placeholder (i18n [:tree/search-term "Suchbegriff"])
                                   :value (:text @filter)
                                   :on-change filter-changed}])}))

(defn tree
  "Tree component for recursive data. Should point to a structure that has the shape `{:tree [{:label \"lbl\", :icon \"css-class\", :id ..., :children [...]}]}`. There will be other values written to the same location (filter, internal states etc.)"
  [opts]
  (let [tree (subscribe [:tree])
        filter (subscribe [:tree/filter])
        state (subscribe [:tree/state])
        hidden? (subscribe [:tree/hidden?])
        sel-plants (subscribe [:selected-plants])]
    (fn [path opts]
      [:div
       [:div.row
        [:div.btn-toolbar
         [:div.btn-group
          [(if (empty? (:text @filter))
             :button
             :button.btn-info)
           {:class "btn btn-default glyphicon glyphicon-filter"
            :title (i18n [:tree/filter "Filter"])
            :on-click #(dispatch [:tree/toggle-filter])}]
          [:button {:class "btn btn-default glyphicon glyphicon-resize-full"
                    :title (i18n [:tree/expand "Auswahl ausklappen"])
                    :disabled (nil? (:active @state))
                    :on-click #(dispatch [:tree/expand-collapse-subtree :expand])}]
          [:button {:class "btn btn-default glyphicon glyphicon-resize-small"
                    :title (i18n [:tree/collapse "Auswahl einklappen"])
                    :disabled (nil? (:active @state))
                    :on-click #(dispatch [:tree/expand-collapse-subtree :collapse])}]
          [:button {:class "btn btn-default glyphicon glyphicon-unchecked"
                    :title (i18n [:tree/drain "Auswahl leeren"])
                    :disabled (empty? (:selected? @state))
                    :on-click #(dispatch [:tree/clear-selection])}]
          [:button {:class "btn btn-default glyphicon glyphicon-check"
                    :title (i18n [:tree/expand-selected "Zeige selektierte Sensoren"])
                    :disabled (empty? (:selected? @state))
                    :on-click #(dispatch [:tree/focus-selected])}]]
         [:span [park/plant-selector]]]]
       (when (:show? @filter)
         [filter-text filter])
       [:div.row {:style {:height "75vh"
                          :overflow-y :auto}}
        [tree-nodes @tree @state @hidden? opts]]])))
