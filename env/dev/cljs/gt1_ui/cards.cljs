(ns gt1-ui.cards
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.interop :refer-macros [$]]
            reagent.ratom
            [reagent.session :as session]
            [gt1-ui.core :as core]
            [gt1-ui.calendar :as cal]
            [gt1-ui.tree :as tree]
            [gt1-ui.table :as table]
            [gt1-ui.chart :as chart]
            [gt1-ui.park :as park]
            gt1-ui.cards.park
            gt1-ui.cards.calendar
            gt1-ui.cards.matrix
            [devtools.core :as devtools])
  (:require-macros
     [devcards.core
      :as dc
      :refer [defcard defcard-doc defcard-rg deftest doc]]))

(defonce initial-setup
  (do
    (enable-console-print!)
    (devtools/set-pref! :install-sanity-hints true) ; for better stacktraces in case of dereferencing nulls
    (devtools/install!)))


(defcard-rg wind-rose
  (let [sectors (range 0 360 30)
        values (repeatedly (count sectors) #(hash-map :n (rand) :v (rand-int 20)))
        sum (apply + (map :n values))
        values (zipmap sectors (map #(update % :n / sum) values))]
    [:div
     [park/wind-rose 50 "foo" values]
     [park/wind-rose 100 "foo" values]
     [park/wind-rose 200 "bar" values]
     [park/wind-rose 400 "baz" values]]))

#_(defcard-rg table 
  [table/table {:headers ["c1" "c2" "c3"]
                :rows (repeatedly 5 #(repeatedly 3 (fn [] (rand-int 10))))}
   {:class "table table-hover table-striped table-bordered"
    :caption [:h4 {:style {:color "red"}} "table caption"]
    :hideable? true}])


(defcard dates
  (let [d1 #inst "2013-01-02"
        d2 #inst "2013-01-02"
        m {d1 true}]
    [d1 d2 (get m d1) (get m d2)]))


(let [state (atom [[1,[0 1 2]]
                   [2,[1 2 3]]
                   [3,[2 3 4]]
                   [4,[.5 2 2.2]]
                   [5 [6 7 10]]])]
  #_(js/setInterval #(swap! state conj [7 [ 1 1 1]]) 2000)

  (defcard-rg dygraph
    (fn [data _]
      [:div {:width 700}
       [chart/line-chart @data {:labels ["Daten"]}]])
    state))


(defcard-rg menu-bar
  [:nav.navbar.navbar-default
   [:ul.nav.navbar-nav
    [:li [:a "test"]]
    [:li.dropdown
     [:a.dropdown-toggle {:data-toggle "dropdown"} "Dropdown" [:span.caret]]
     [:ul.dropdown-menu
      [:li>a "foo"]
      [:li>a "bar"]
      [:li.divider {:role "separator"}]
      [:li>a "baz"]]]]])

(defcard-rg tree
  (fn [tree-atom _]
    [tree/tree tree-atom (atom {}) {:callback println}])
  (atom [{:label "root"
          :expanded? true
          :children [{:label "c1"
                      :expanded? true}
                     {:label "c2"
                      :children [{:label "cc1"}]}
                     {:label "custom"
                      :icon "glyphicon-trash"}]}])
  {:inspect-data true})



; remove static content visible without javascript
(reagent/render [:div] (.getElementById js/document "app"))

;; remember to run 'lein figwheel devcards' and then browse to
;; http://localhost:3449/cards

