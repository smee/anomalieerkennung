(ns gt1-ui.handler
  (:require [clj-time
             [core :as time]
             [format :as tf]]
            [clojure.core.memoize :as memo]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [compojure.core :refer [GET POST defroutes context routes]]
            [compojure.route :refer [not-found resources]]
            [environ.core :refer [env]]
            [gt1-ui.data
             [availability :as avail]
             [csv :as csv]
             [db :as db]
             [etl :as etl]
             [metadata :as md]]
            [gt1-ui.defaults :as dflt]
            [gt1-ui.util.transit :as tr]
            [gt1-ui.util.tree :as tree]
            [gt1-ui.version :as version]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-js include-css]]
            [org.clojars.smee.map :refer [map-values]]
            [overtone.at-at :as sched]
            [prone.middleware :refer [wrap-exceptions]]
            [ring.middleware
             [cache :as cache]
             [defaults :refer [secure-site-defaults site-defaults wrap-defaults]]
             [format :refer [wrap-restful-format]]
             [gzip :as gzip]
             [reload :refer [wrap-reload]]]
            [ring.util.response :refer [response]]
            [clojure.edn :as edn]))

(declare conn status-codes)
(def dev? (= "true" (env :dev)))
(def demo? (= "true" (env :demo)))
(def min-date (if demo? (time/date-time 2016) (time/date-time 2014)))

(defn plant-name
"For demos, rename Globaltech specific names."
  [^String s]
  (let [[_ pre idx] (re-find #"(GT|WEA)(\d\d)" s)
        idx (Integer/parseInt idx)]
    (if (= pre "GT")
      (format "WEA%02d" (- idx 68))
      (format "GT%02d" (+ idx 68)))))
;; replace plant renaming function if we are in DEMO mode
(alter-var-root #'plant-name #(if demo? % identity))

(defn- get-structure
  [plant]
  (-> conn
      (db/get-plant-metadata plant)
      (update :tree (comp tree/add-ids tree/munge-tree tree/add-ids))
      (assoc :table (let [rows (map #(select-keys % [:label :description :unit :dimension :component :column-name]) (filter #(= "avg" (:statistics %)) (db/get-series-metadata conn plant)))]
                      {:headers (mapv name (keys (first rows)))
                       :rows (mapv vals rows)}))))
(alter-var-root #'get-structure memo/lu :lu/threshold 10)

(defn- get-plants []
  (->> "select name,latitude,longitude from plants order by name"
       (db/adhoc conn)
       (take-last (if demo? 12 Long/MAX_VALUE))
       (mapv #(update % :name plant-name))))

(def css (cond
           (and dev? demo?) (include-css "css/bootstrap-3.3.5.css"
                                         "css/site.css"
                                         "css/colorbrewer.css"
                                         "css/demo.css"
                                         "css/reagent-forms.css")
           (and dev? (not demo?)) (include-css "css/bootstrap-3.3.5.css"
                                              "css/gt1.css"
                                              "css/site.css"
                                              "css/colorbrewer.css"
                                              "css/reagent-forms.css")
           demo? (include-css "css/demo.min.css")
           :else (include-css "css/production.min.css")))

(def mount-target [:div#app])

(defn loading-page []
  (let [today (time/today-at-midnight)
        last-month (time/minus today (time/months 1))]
    (html
     [:html
      [:head
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport"
               :content "width=device-width, initial-scale=1"}]
       [:base {:href "/"}]
       css]
      [:body
       mount-target
       [:footer
        [:div#footer
         [:div#footer-bottom
          [:div#footer-meta
           [:p (if demo? "© Demo, 2016" "© Global Tech I, 2016")]]]]]
       [:div {:style "display:none;"}
        [:pre#security-token ring.middleware.anti-forgery/*anti-forgery-token*]
        [:pre#initial-state 
         (pr-str {:version (version/get-version)
                  :plants (get-plants)
                  :structure (get-structure "GT01")
                  :chart {:zoom nil
                          :errors? true
                          :modes? false
                          :overall {:start-date {:year (time/year last-month) :month (time/month last-month) :day (time/day last-month) :hour 0 :minute 0}
                                    :end-date {:year (time/year today) :month (time/month today) :day (time/day today) :hour 0 :minute 0}}}
                  :tree {:active {:selected {} :active nil}                                
                         :filter {:show? false
                                  :searches []
                                  :text ""}}})]]
       (include-js "js/app.js")]])))

(def cards-page
  (html
   [:html
    [:head
     [:meta {:charset "utf-8"}]
     css]
    [:body
     mount-target
     (include-js "js/app_devcards.js")]]))

(defn fetch-chart-data [series {:keys [from to]} plants requested?]
  (let [data (when (requested? :values)
               (db/rolled-up-values-in-time-range conn
                                                  plants
                                                  series
                                                  from
                                                  to
                                                  dflt/num-data-points))
        ;; insert trace data if the requested time interval is smaller than a few hours
        data (if (and (requested? :values)
                      (<= (time/in-hours (time/interval from to)) 2))
               (sort-by first (into data (db/traces conn plants series from to)))
               data)]
    (response
     (cond-> {}
       (requested? :values) (assoc :values data)
       (requested? :errors) (assoc :errors (map-values plant-name
                                                       (fn [errors]
                                                         (for [{c :component e :error-id :as m} errors]
                                                           (-> m
                                                               (assoc :component {:id c :label (get-in status-codes [c :name] "unbekannt")})
                                                               (assoc :error (get-in status-codes [c :codes e] {:id e :label "unbekannt" :description "unbekannter Fehler" :code "???" :short-code "???"}))
                                                               (dissoc :error-id))))
                                                       (db/errors conn plants from to)))
       (requested? :modes) (assoc :modes (avail/detailed-modes conn (first plants) (time/latest min-date from) (time/earliest (time/now) to)))))))


;;;;;;; request handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn multi-args
  "If a request has multiple values per query parameter we will
get a map with numeric strings resembling the order of arguments.
Create a sorted vector with argument values instead."
  [m]
  (vec
   (for [i (range (count m))]
     (get m (str i)))))

(def fmt (tf/formatter time/utc "yyyy-MM-dd"  "yyyy-MM-dd'T'HH:mm:ss'Z'"))
(defn- parse-date [date]
  (tf/parse fmt date))

(defn- parse-dates [m]
  (let [[from to] (multi-args m)
        from (parse-date from)
        from (if demo? (time/latest from min-date) from)
        to (parse-date to)
        to (if demo? (time/earliest to (time/now)) to)
        from' (time/earliest from to)
        to' (time/latest from to)]
    {:from from'
     :to to'}))

(defn validate-dates [dates]
  (when (not= 2 (count dates))
    {:status 400 :body "invalid date range"}))

(defn validate-not-empty [series]
  (when (empty? series)
    {:status 400 :body "no series names specified"}))


;; define multiple route sets, can be composed differently if DEMO mode is enabled
(defroutes readonly-routes    
  #_(GET "/matrix" [] 
      (response gt1-ui.local.lasso/adj-m))
  (context "/db" []
    (GET "/plants" [] (response (get-plants)))
    (GET "/aggregated-modes" [dates plants]
      (let [modes (avail/aggregated-modes conn)
            modes (if demo? (map-values plant-name identity modes) modes)]
        (response modes)))
    (GET "/anomalies" [plant]
      (response (db/algorithm-results conn (plant-name plant))))  
    (GET "/data" [series dates plants errors? modes? values?]
      (let [requested? (cond-> #{}
                         (= "true" values?) (conj :values)
                         (= "true" (str/lower-case errors?)) (conj :errors)
                         (and (= 1 (count plants))
                              (= "true" (str/lower-case modes?))) (conj :modes))
            plants (mapv plant-name (multi-args plants))]
        (or (validate-dates dates)
            (and (requested? :values)
                 (validate-not-empty series))
            (fetch-chart-data (multi-args series)
                              (parse-dates dates)
                              plants
                              requested?))))
    (GET "/windstats" [dates plants]
      (let [{:keys [from to]} (parse-dates dates)]
        (response (db/fetch-wind-stats conn (multi-args plants) from to))))
    (context "/plants/:plant" [plant]
      (GET "/" []
        (response (get-structure (plant-name plant)))))))

(defroutes production-routes
  (GET "/cards" [] cards-page)
  (POST "/process" [] (etl/run-algorithms! conn))
  (POST "/anomalies/annotations" [plant timestamp text]
         (db/store-annotation! conn plant timestamp text)
         {:status 200})
  (GET "/anomalies/annotations/:plant" [plant]
        (response (db/annotations conn plant))))

(defroutes base-routes
  (GET "/version" [] (version/get-version))
  (resources "/")
  (GET "/*" [] (loading-page)))

(def app
  (let [handler (-> (if demo?
                      (routes #'readonly-routes #'base-routes)
                      (routes #'readonly-routes #'production-routes #'base-routes))
                    ;(cache/wrap-control-max-age {200 (* 60 60)}) ; cache all 200 responses for one hour
                    (wrap-restful-format :formats [:transit-json :edn]
                                         :params-options {:transit-json {:handlers tr/readers}}
                                         :response-options {:transit-json {:handlers tr/writers}})
                    (wrap-defaults site-defaults)                    
                    gzip/wrap-gzip)]
    (if dev?
      (-> handler wrap-exceptions wrap-reload)
      handler)))


(when (not *compile-files*)
  (defonce initialization
    (do
      (def status-codes (md/parse-status-codes (io/file (env :input-folder) "M5000-de.xml")))
      ;; connection pool to database
      (def conn (let [n (env :db-name)
                          db-url (env :database-url) ; set by docker
                          spec (db/default-spec n db-url)]
                      (db/create-connection-pool! n spec)))
      
      (def task-pool (sched/mk-pool))
      ;; start periodic import of new measurement data in production
      (let [import (env :import-folder)
            input (env :input-folder)
            import-fn #(do (println "Starting file import at" (time/now) "from folder" input)
                           (try
                             (let [files (etl/find-importable-files conn input)]
                               (printf "found %d new files\n" (count files)) (flush)
                               (when (not-empty files)
                                 (etl/import! conn files import)
                                 (println "Import finished. reconstructing error intervals")
                                 (etl/reconstruct-error-intervals conn import)
                                 (println "Reconstruction finished. Rows:" (db/adhoc conn "select count(*) from errors"))
                                 (println "filling cache...")
                                 (avail/fill-avail-cache! conn min-date (time/plus (time/now) (time/days 1)))
                                 (println "running anomaly detections")
                                 (etl/run-algorithms! conn)
                                 (println "done!")))
                             (catch Exception e
                               (.printStackTrace e))))]
        
        (future
          ;; initialize database metadata
          (etl/initialize-database! conn (env :db-name) (env :input-folder))
          ;; there may have been an error with repopulating the error table. make sure it is filled correctly
          (when (zero? (:c (first (db/adhoc conn "select count(*) as c from errors"))))
            (etl/reconstruct-error-intervals conn import))
          (avail/fill-avail-cache! conn min-date (time/plus (time/now) (time/days 1)))
          (when (not demo?)
            (println "scheduling import from" input "into" import)
            (sched/every (time/in-millis (time/minutes 30))
                         import-fn
                         task-pool)))))))
