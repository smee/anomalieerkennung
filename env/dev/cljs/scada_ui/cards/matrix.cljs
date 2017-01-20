(ns scada-ui.cards.matrix
  (:require [scada-ui.components.matrix :as m]
            [ajax.core :refer [GET] :include-macros true])
  (:require-macros
     [devcards.core
      :as dc
      :refer [defcard defcard-doc defcard-rg deftest doc]]))

(defcard-rg simple-matrix
  (let [names (map #(str "x" %) (range 50))
        col-names (map #(str "x" %) (range 100))
        adm (reduce (fn [m n]
                      (assoc m n (distinct (repeatedly 10 #(rand-nth col-names))))) {} names)]
    [m/matrix names adm]))

(let [m (atom {})]
  (GET "/matrix" :handler #(reset! m %))

  (defcard-rg lasso-model    
    (fn [atm _]
      [m/matrix (sort (keys @atm)) @atm {:cell-size 3}])
    m)

  (defcard-rg lasso-model-spantree-graph
    "reverse span tree of a target series as a force directed graph"
    (fn [atm _]
      (let [n "1/1/25/avg"
            [ks m] (m/reverse-span-tree @atm n)]
        [:div
         [:h4 n]
         [m/directed-graph m]]))
    m)
  
  (defcard-rg lasso-model-spantree
    "reverse span tree of a target series"
    (fn [atm _]
      (let [n "1/1/25/avg"
            [ks m] (m/reverse-span-tree @atm n)]
        [:div
         [:h4 n]
         [m/matrix ks m {:cell-size 12 :background? false}]]))
    m))
