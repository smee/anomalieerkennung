(ns scada-ui.components.table
  (:require [cljs-time.core :as time :refer [date-time]]
            [cljs-time.format :as tf]
            [clojure.walk :as walk]
            [re-frame-datatable.core :as dt]
            [reagent.core :as reagent]
            [re-frame.core :refer [reg-event-db reg-sub dispatch subscribe]]
            [scada-ui.components.bootstrap :as bs]
            [scada-ui.format :as f]
            [scada-ui.i18n :refer [i18n]]
            [scada-ui.pages.common :as pc]
            [clojure.string :as str]))


(defn- duration-str [interval]
  (let [s (tf/unparse-duration interval)]
    (-> s
        (str/replace "days" (i18n [:time/days]))
        (str/replace "day" (i18n [:time/day]))
        (str/replace "hours" (i18n [:time/hours]))
        (str/replace "hour" (i18n [:time/hour]))
        (str/replace "minutes" (i18n [:time/minutes]))
        (str/replace "minute" (i18n [:time/minute]))
        (str/replace "seconds" (i18n [:time/seconds]))
        (str/replace "second" (i18n [:time/second])))))

(defn pagination [db-id data-sub]
  (let [pagination-state (subscribe [::dt/pagination-state db-id data-sub])]
    (fn []
      (let [{:keys [::dt/cur-page ::dt/pages]} @pagination-state
            total-pages (count pages)
            next-enabled? (< cur-page (dec total-pages))
            prev-enabled? (pos? cur-page)
            last-idx (second (last pages))
            n (second (last pages))]
        (when (> total-pages 1)
          [:div {:style {:text-align "center"}}
           [bs/Pagination {:bs-size "sm"
                           :items total-pages
                           :first true
                           :prev true
                           :next true
                           :last true
                           :max-buttons 7
                           :active-page (inc cur-page)
                           :on-select #(dispatch [::dt/select-page db-id @pagination-state (dec %)])}]
           [:div.pull-right (f/format "%d/%d (%d)" (inc cur-page) total-pages n)]])))))

(def fmt (tf/formatter "dd.MM.yyyy HH:mm:ss"))

(defn t [dt]
  (tf/unparse fmt dt))

(defn multiple-plants? [vs]
  (< 1 (count (distinct (map :plant vs)))))


(defn error-table [db-id error-subv opts]
  (let [multi? (multiple-plants? @(subscribe error-subv))]
    [:div
     [dt/datatable db-id error-subv
      (->> [(when multi?
              {::dt/column-key [:plant]   ::dt/column-label (i18n [:plant "Anlage"]) ::dt/sorting {::dt/enabled? true}})
            {::dt/column-key [:timestamp] ::dt/column-label (i18n [:start "Start"])          ::dt/render-fn (fn [s] [:span (t s)]) ::dt/sorting {::dt/enabled? true}}
            {::dt/column-key [:end]       ::dt/column-label (i18n [:time/duration "Dauer"])  ::dt/render-fn (fn [end {s :timestamp}] [:span {:title (t end)} (duration-str (time/interval s end))])}
            {::dt/column-key [:component] ::dt/column-label (i18n [:component "Komponente"]) ::dt/render-fn (fn [c] [:span c])}
            {::dt/column-key [:id]        ::dt/column-label (i18n [:error "Fehler"])         ::dt/render-fn (fn [id row] [:span {:title (:description row)} (or (:label row) id)])}
            {::dt/column-key [:code]      ::dt/column-label (i18n [:code "Code"])            ::dt/render-fn (fn [code row] [:span {:title code} (:short-code row)])}]
           (filterv identity))
      (cond-> {::dt/table-classes ["table" "table-compact" "table-striped"]
               ::dt/empty-tbody-component (fn [] [:span.bg-success [:i {:class "glyphicon glyphicon-ok"}] (i18n [:modes-page/no-errors])])}
        (:per-page opts) (assoc ::dt/pagination {::dt/enabled? true
                                                 ::dt/per-page (:per-page opts)}))]
     [pagination db-id error-subv]]))



(defn- changed-value-fmt [v {:keys [type old new unit divisor] :as change}]
  (let [translate {1 [:span {:style {:color "green"}} (i18n [:on "an"])]
                   0 [:span {:style {:color "red"}} (i18n [:off "aus"])]}
        digital? (or (= type "digital")
                     (and (translate old) (translate new)))]
    (if digital?
      (translate v v)
      [:span (f/format-number (* v (or divisor 1)) 3) unit])))

(defn changes-table [db-id changes-subv]
  (let [multi? (multiple-plants? @(subscribe changes-subv))]
    [:div
     [dt/datatable db-id changes-subv
      (->> [(when multi?
              {::dt/column-key [:plant] ::dt/column-label (i18n [:plant "Anlage"]) ::dt/sorting {::dt/enabled? true}})
            {::dt/column-key [:timestamp] ::dt/column-label (i18n [:time/time "Zeit"]) ::dt/render-fn (fn [s] [:span (t s)]) ::dt/sorting {::dt/enabled? true}}
            {::dt/column-key [:user] ::dt/column-label (i18n [:user "Anwender"]) ::dt/sorting {::dt/enabled? true}}
            {::dt/column-key [:type] ::dt/column-label (i18n [:kind "Art"]) ::dt/sorting {::dt/enabled? true}}
            {::dt/column-key [:label] ::dt/column-label (i18n [:label "Bezeichnung"]) ::dt/render-fn (fn [lbl row] [:span {:title (:description row)} (or lbl (:id row))])}
            {::dt/column-key [:old] ::dt/column-label (i18n [:old "Alt"]) ::dt/render-fn changed-value-fmt}
            {::dt/column-key [:new] ::dt/column-label (i18n [:new "Neu"]) ::dt/render-fn changed-value-fmt}]
           (filterv identity))
      {::dt/table-classes ["table" "table-compact" "table-striped"]
       ::dt/pagination {::dt/enabled? true ::dt/per-page 10}}]
     [pagination db-id changes-subv]]))


(defn modes-table [db-id modes-subv]
  (let [multi? (multiple-plants? @(subscribe modes-subv))]
    [:div
     [dt/datatable db-id modes-subv
      (->> [(when multi?
              {::dt/column-key [:plant]   ::dt/column-label (i18n [:plant "Anlage"]) ::dt/sorting {::dt/enabled? true}})
            {::dt/column-key [:timestamp] ::dt/column-label (i18n [:start "Start"]) ::dt/render-fn (fn [s] [:span (t s)]) ::dt/sorting {::dt/enabled? true}}
            {::dt/column-key [:duration]  ::dt/column-label (i18n [:time/duration "Dauer"]) ::dt/render-fn (fn [dur row] [:span {:title (t (:end row))} (duration-str dur)])}
            {::dt/column-key [:color]     ::dt/column-label (i18n [:mode "Zustand"]) ::dt/render-fn (fn [color row] [:span [pc/color-cube color] (:name row)])}]
           (filterv identity))
      {::dt/table-classes ["table" "table-compact" "table-striped"]
       ::dt/pagination {::dt/enabled? true ::dt/per-page 10}}]
     [pagination db-id modes-subv]]))

(defn lasso-table [{attrs :attributes}] ;; TODO convert to dt/datatable
  (let [id->label @(subscribe [:name->label])]
    [:table {:class "table table-compact table-striped"}
     [:thead
      [:tr
       [:th (i18n [:column "Spalte"])]
       [:th (i18n [:sensor "Sensor"])]
       [:th (i18n [:lasso/relative-weight "Relatives Gewicht"])]]]
     [:tbody
      (doall
       (for [[k {:keys [importance]}] (reverse (sort-by (comp :importance second) attrs))]
         ^{:key k}
         [:tr
          [:td k]
          [:td (id->label (str/replace k "/avg" ""))]
          [:td (f/format-number (* 100 importance) 2) "%"]]))]]))
