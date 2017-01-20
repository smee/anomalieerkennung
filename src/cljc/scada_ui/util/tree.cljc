(ns scada-ui.util.tree
  (:require [clojure.walk :as walk]
            [clojure.zip :as zip]
            #?(:cljs [goog.string]))
  #?(:clj (:import java.util.UUID)))

(defn nodes [tree]
  (filter map? (tree-seq #(or (vector? %) (map? %)) #(if (map? %) (:children %) (seq tree)) tree)))

(defn random-id []
  #?(:cljs (random-uuid)
     :clj (UUID/randomUUID)))

(defn add-ids
"Add unique identifiers to each intermediate tree node, keeps leaves' ids."
  [tree]
  (walk/postwalk #(if (and (map? %) (not (:id %)))
                    (assoc % :id (random-id))
                    %) tree))
(defn munge-tree
"Take a tree describing components, returns an enhanced tree with two children:
the original tree and a tree with subtrees per physical entity."
  [tree]
  (let [types (->> tree
                   nodes
                   (map :dimension)
                   (remove nil?)
                   distinct
                   sort)
        filter-tree-by (fn [type tree]
                         (walk/postwalk
                          (fn [x]
                            (cond
                              (map? x) (let [children (vec (remove nil? (:children x)))
                                             children? (not-empty children)]
                                         (cond
                                           (= type (:dimension x)) (dissoc x :children)
                                           children? (assoc x :children children)))
                              (and (vector? x) (every? (some-fn nil? map?) x)) (vec (remove nil? x))
                              :else x))
                          tree))]
   [{:label "Komponenten"
      :children tree}
     {:label "Physikalisch"
      :children (vec (for [t types] {:label t :children (filter-tree-by t tree)}))}]))

(defn tree-zip [tree]
  (zip/zipper coll?
              #(cond
                 (map? %) (:children %)
                 (vector? %) (seq %))
              (fn [node children]
                (cond
                  (nil? node) nil
                  (map? node) (assoc node :children (vec children))
                  (vector? node) (into [] children)
                  :else (throw (ex-info "unknown node type" {:node node :children children}))))
              tree))

(defn ids-on-path
"All ids on all paths to any node in `nodes`"
  [tree ids]
  (let [target? (set ids)]
    (loop [loc (tree-zip tree), parents #{}]
      (cond
        (zip/end? loc) parents
        (target? (:id (zip/node loc))) (recur (zip/next loc) (into parents (map :id (filter map? (zip/path loc)))))
        :else (recur (zip/next loc) parents)))))

(defn find-by-id [tree id]
  (first (filter #(= id (:id %)) (nodes tree))))

(defn expand-path-to [tree nodes]
  (let [in-ids? (into #{} (map :id nodes))]
    (walk/postwalk #(if (map? %)
                      (assoc % :expanded? (or (in-ids? (:id %)) (some :expanded? (:children %))))
                      %)
                   tree)))

(defn by-attribute [tree attribute]
  (let [nodes (nodes tree)]
    (dissoc (zipmap (map #(get % attribute) nodes) nodes) nil)))
