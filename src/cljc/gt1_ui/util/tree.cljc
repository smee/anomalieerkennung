(ns gt1-ui.util.tree
  (:require [clojure.walk :as walk]
            [clojure.zip :as zip]
            #?(:cljs [goog.string]))
  #?(:clj (:import java.util.UUID)))

(defn set-expanded!
"Recursively set each node's `:expanded?` flag to a fix value."
  [root-node start-node value]
  (walk/postwalk #(if (and (map? %)
                           (= (:id %) (:id start-node)))
                    (walk/postwalk (fn [x]
                                     (if (map? x)
                                       (assoc x :expanded? value)
                                       x))
                                   %) 
                    %)
                 root-node))

(defn hide-non-matching
"Set `:hidden?` flag on each tree node that either does not contain all search strings in `searches`
or has any non-hidden children."
  [nodes searches]
  (walk/postwalk (fn [form]
                   (if (map? form)
                     (assoc form :hidden? (and (not-every? #(#?(:cljs js/goog.string.caseInsensitiveContains
                                                                :clj .contains) (:label form) %) searches)
                                               (every? :hidden? (:children form))))
                     form))
                 nodes))

(defn- random-id []
  #?(:cljs (random-uuid)
     :clj (UUID/randomUUID)))

(defn add-ids
"Add unique identifiers to each tree node."
  [tree]
  (walk/postwalk #(if (and (map? %) (not (:id %)))
                    (assoc % :id (random-id))
                    %) tree))
(defn munge-tree
"Take a tree describing components, returns an enhanced tree with two children:
the original tree and a tree with subtrees per physical entity."
  [tree]
  (let [types (->> tree
                   (tree-seq coll? #(or (:children %) (seq %)))
                   (filter map?)
                   (map :dimension)
                   (remove nil?)
                   distinct
                   sort)
        filter-tree-by (fn [type tree]
                         (walk/postwalk
                          (fn [x]
                            (cond
                              (map? x) (when (or (= type (:dimension x))
                                                 (not-empty (remove nil? (:children x)))) (assoc x :children (vec (remove nil? (:children x)))))
                              (vector? x) (vec (remove nil? x))
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

(defn path-to
"Sequence of nodes in a tree to an arbitrarly nested node `node`."
  [tree node]
  (loop [loc (tree-zip tree)]
    (when-not (zip/end? loc)
      (if (= (:id (zip/node loc)) (:id node))
        (concat (filter map? (zip/path loc)) [node])
        (recur (zip/next loc))))))

(defn find-by-id [tree id]
  (loop [loc (tree-zip tree)]
    (when-not (zip/end? loc)
      (let [n (zip/node loc)]
        (if (= (:id n) id)
          n
          (recur (zip/next loc)))))))

(defn expand-path-to [tree nodes]
  (let [in-ids? (into #{} (map :id nodes))]
    (walk/postwalk #(if (map? %)
                      (assoc % :expanded? (or (in-ids? (:id %)) (some :expanded? (:children %))))
                      %)
                   tree)))

(defn by-attribute [tree attribute]
  (loop [loc (tree-zip tree), res []]
    (if (zip/end? loc)
      (zipmap (map #(get % attribute) res) res)
      (let [n (zip/node loc)]
        (recur (zip/next loc) (if (and (map? n) (get n attribute))
                                (conj res n)
                                res))))))
(defn tree->indexed [tree]
  (if (map? tree)
    tree
    (into {} (for [node tree]
               [(:index node) (assoc (tree->indexed (or (:children node) node))
                                     :label (:label node))]))))
