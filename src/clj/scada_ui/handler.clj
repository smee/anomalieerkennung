(ns scada-ui.handler
  (:require [clj-time
             [core :as time]
             [format :as tf]
             [periodic :as tp]]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [taoensso.sente.packers.transit :as sente-transit]
            [clojure
             [edn :as edn]
             [set :as set]
             [string :as str]]
            [clojure.core.memoize :as memo]
            [clojure.java.io :as io]
            [compojure
             [core :refer [context defroutes GET POST routes]]
             [route :refer [not-found resources]]]
            [environ.core :refer [env]]
            [scada-ui
             [lasso :as gl]
             [version :as version]
             [algorithms :as algo]]
            [scada-ui.data
             [csv :as csv]
             [db :as db]
             [etl :as etl]
             [structure :as struc]]
            [scada-ui.util             
             [transit :as tr]
             [tree :as tree]]
            [hiccup.page :refer [html5 include-css include-js]]
            [org.clojars.smee.map :refer [map-values]]
            [overtone.at-at :as sched]
            [prone.middleware :refer [wrap-exceptions]]
            [ring.middleware
             [cache :as cache]
             [cljsjs :refer [wrap-cljsjs]]
             [defaults :refer [secure-site-defaults site-defaults wrap-defaults]]
             [format :refer [wrap-restful-format]]
             [gzip :as gzip]
             [reload :refer [wrap-reload]]]
            [streaming.fading :as fade]
            [ring.util.response :refer [response]]
            [timeseries.features :as feat]
            [streaming.protocols :as sp]
            [scada-ui.util :as util]))

(def fmt (tf/formatter time/utc "yyyy-MM-dd"  "yyyy-MM-dd'T'HH:mm:ss'Z'"))

(declare conn status-codes analog-parameters digital-parameters)
(def dev? (= "true" (env :dev)))
(def demo? (= "true" (env :demo)))
(def min-date (if demo? (time/date-time 2015 9 1) (time/date-time 2014)))
(defn max-date [] (if demo? (time/date-time 2016 5 1) (time/with-time-at-start-of-day (time/now))))

(defn plant-name
"For demos, rename Globaltech specific names."
  [^String s]
  (let [[_ pre idx] (re-find #"(GT|WEA)(\d\d)" s)
        idx (Integer/parseInt idx)]
    (if (= pre "GT")
      (format "WEA%02d" idx)
      (format "GT%02d" idx))))
;; replace plant renaming function if we are in DEMO mode
(alter-var-root #'plant-name #(if demo? % identity))

(defn- get-plants []
  (let [vs (sort-by :name (struc/plants))
        [min-lat max-lat] (apply (juxt min max) (map :latitude vs))
        [min-lon max-lon] (apply (juxt min max) (map :longitude vs))
        n (count vs)]
    (if demo?
      (vec (map-indexed
           (fn [i {p :name t :type}]
             (let [row (int (/ i 8))
                   col (mod i 8)
                   h (* 0.5 (- max-lat min-lat))
                   w (- max-lon min-lon)]
               {:name (plant-name p)
                :type t
                :latitude (- max-lat (* row h))
                :longitude (+ min-lon (* col w) (if (odd? row) (/ w 2) 0))})) vs))
      (vec vs))))

(def css (if dev?
           (include-css "css/bootstrap-paper.css"
                        "css/site.css"
                        "css/colorbrewer.css"
                        ;;"/cljsjs/vis/vis.inc.css"
                        "/cljsjs/react-day-picker/react-day-picker.inc.css")
           (include-css "css/production.min.css" "/cljsjs/react-day-picker/react-day-picker.inc.css")))

(def mount-target [:div#app])

(defn loading-page []
  (html5
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
        [:p "© Steffen Dienst, 2016"]]]]]
    [:div {:style "display:none;"}
     [:pre#security-token ring.middleware.anti-forgery/*anti-forgery-token*]
     [:pre#demo (str demo?)]
     [:pre#versions (pr-str (struc/plant-spec-versions))]
     [:pre#plants (pr-str (get-plants))]
     [:pre#today (tf/unparse fmt (max-date))]]
    (include-js "js/app.js")]))

(def cards-page
  (html5
   [:head
    [:meta {:charset "utf-8"}]
    css]
   [:body
    mount-target
    (include-js "js/app_devcards.js")]))

(defn- memoize!
"Uses core.memo to memoize functions, but takes care not to wrap a function twice
(for example due to hotloading in the REPL)."
  [value memo-fn & args]
  (let [value (if (memo/memoized? value) (memo/memo-unwrap value) value)]
    (apply memo-fn value args)))

(defn- create-power-predictions [{:keys [plant options]}]
  (let [{:keys [start end]} (:dates options)]
    (algo/estimate-power-curve conn plant start end (assoc options :alpha (fade/half-life->alpha (:half-life options 1000))))))

(alter-var-root #'create-power-predictions memoize! memo/lru :lru/threshold 20)

(defn- rollup [segment]
  (let [n (count segment)]
    (if (> n 1)
      (let [ts (ffirst segment)
            vs (map second segment)
            mi (reduce min (map first vs))
            ma (reduce max (map last vs))
            avg (/ (reduce + (map second vs)) n)]
        [ts [mi avg ma]])
      (first segment))))

(defn- align [timestamps data]
  (loop [ts (next timestamps), vs data, segment [], res []]
    (let [crnt-ts (ffirst vs)]
      (cond
        (or (nil? ts) (nil? vs)) (vec (remove nil? (conj res (rollup segment))))
        (time/after? (first ts) crnt-ts) (recur ts (next vs) (conj segment (first vs)) res)
        :else (recur (next ts) vs [] (conj res (rollup segment)))))))

(defn- ignore-spikes 
  "Remove spikes over `threshold` in `ratios` (if there is just one consecutive value over the threshold). 
  Replace by average."
  ([ratios] (ignore-spikes ratios 0.0))
  ([ratios threshold]
   (let [inner (->> ratios
                    (partition 3 1)
                    (map (fn [[a b c]] 
                           (if (and (<= a threshold) 
                                    (> b threshold) 
                                    (<= c threshold))
                               (/ (+ a c) 2.0) 
                               b))))]
     (cons (first ratios)
           (concat inner (take-last 1 ratios))))))

(defn power-predictions
"Create a power range prediction using algorithms/estimate-power-curve. Uses the given timestamps
  to make sure, we are aligned with sensor values fetched from the database"
  [series timestamps]
  (reduce (fn [res s]
            (let [fst-ts (first timestamps)
                  lst-ts (last timestamps)
                  data (->> (create-power-predictions s)
                            (drop-while #(time/after? fst-ts (first %)))
                            (take-while #(time/before? (first %) lst-ts)))
                  f (if (:ignore-spikes? (:options s)) ignore-spikes identity)
                  ts (map first data)
                  diffs-min (map (fn [ts diff] [ts [diff diff diff]])
                             ts
                             (f (for [[ts [min avg]] data] (if (< avg min) (- min avg) 0))))
                  #_#_diffs-avg (map (fn [ts diff] [ts [diff diff diff]])
                                 (map first data)
                                 (f (for [[ts [min avg max]] data]
                                      (if (< avg min) (- (+ min (* 0.5 (- max min))) avg) 0))))]
              (assoc res
                     (assoc s :series "powerprediction") (align timestamps data)
                     (assoc s :series "powerlosses") (align timestamps diffs-min)
                     #_#_(assoc s :series "powerlosses-avg") (align timestamps diffs-avg))))
          {}
          series))

(defn- create-model-predictions
"Load LASSO regression model, generate predictions for large time interval. The results should be cached for interactive
views, because running the model takes several seconds."
  [{:keys [algorithm series start end plant] :as req}]
  (let [{opts :options ls :learn-start alg :algorithm} (db/algorithm-by-id conn algorithm)
        alg (gl/zoom-to alg series true)
        columns (set (mapcat feat/find-stems (sp/input-names alg)))]
    (gl/apply-models conn columns
                     (fn [predictions row]
                       (let [v (get (sp/predict alg row) series)]
                         (conj predictions [(get row "timestamp") [v v v]])))
                     [] plant (time/latest ls start) end opts)))

(alter-var-root #'create-model-predictions memoize! memo/lru :lru/threshold 20)

(defn model-predictions [reqs timestamps]
  (reduce (fn [res req]
            (let [fst-ts (first timestamps)
                  lst-ts (last timestamps)
                  data (->> (create-model-predictions req)
                            (drop-while #(time/after? fst-ts (first %)))
                            (take-while #(time/before? (first %) lst-ts)))]
              (assoc res req (align timestamps data)))) {} reqs))


(defn fetch-chart-data
  [series from to requested?] ;; TODO keep timestamps per from/to pair, needed to load additional power/model predictions with same time granularity
  (let [by-type (group-by :type series)
        pred-req (:power by-type)
        segments 200
        data (if (not (requested? :values))
               {}
               (let [data (db/rolled-up-values conn (:sensor by-type) from to segments (requested? :only-production))
                     timestamps (if (not-empty data)
                                  (map first (first (vals data)))
                                  (tp/periodic-seq from to (time/seconds (int (/ (time/in-seconds (time/interval from to)) segments)))))
                     power (power-predictions pred-req timestamps)
                     ;; add LASSO model predictions
                     predictions (model-predictions (:lasso by-type) timestamps)]
                 (into (into data power) predictions)))      
        plants (distinct (map :plant series))
        pn-list (partial mapv #(update % :plant plant-name))
        pn-map (partial map-values #(update % :plant plant-name) identity)]
    (cond-> {:values (pn-map data)}
      (requested? :traces) (assoc :traces (pn-map (db/traces conn series from to)))
      (requested? :errors) (assoc :errors (pn-list (db/errors conn plants from to)))
      (requested? :modes) (assoc :modes (pn-list (db/modes conn plants from to)))
      (requested? :changes) (assoc :changes (mapv (if demo? #(update % :user (fn [u] (str (subs u 0 1) "..."))) identity) (pn-list (db/parameter-changes conn plants from to)))))))

(defn aggregated-modes [plants from to]
  (db/park-modes-stats (db/aggregated-modes conn plants from to)))
(alter-var-root #'aggregated-modes memo/lru :lru/threshold 3) ;; will only change once per day

;;;;;;;;;;;;; sensor limits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fetch-limits [type]
  (let [alg (:algorithm (first (db/algorithms-by conn {:type type})))]
    (when alg
      (algo/threshold-validator->limits alg))))

(defn store-limits! [type limits]
  (let [all-plants (map :name (filter #(= (:type %) type) (get-plants)))
        existing-algs (db/algorithms-by conn {:type type})
        missing-plants (set/difference (set all-plants) (set (map :plant existing-algs)))
        algorithm (algo/thresholds-validator limits)
        learn-start (time/date-time 2014) ;; TODO external parameter!
        learn-end learn-start]
    (doseq [{:keys [id]} existing-algs]
      (db/update-algorithm! conn id algorithm)
      (db/delete-algorithm-state! conn id)
      (db/delete-algorithm-results! conn id))
    (doseq [plant missing-plants]
      (db/store-algorithm! conn plant learn-start learn-end algorithm type {}))
    (future
      (println "re-running thresholds for" type)
      (algo/run-algorithms! conn type)
      (println "done thresholds."))))

;;;;;;; request handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn multi-args
  "If a request has multiple values per query parameter we will
get a map with numeric strings resembling the order of arguments.
Create a sorted vector with argument values instead."
  [m]
  (vec
   (for [i (range (count m))]
     (get m (str i)))))

(defn- parse-date [date]
  (tf/parse fmt date))

(defn restrict-dates [{:keys [from to]}]
  (let [from (if demo? (time/latest from min-date) from)
        to (if demo? (time/earliest to (time/now)) to)
        from' (time/earliest from to)
        to' (time/latest from to)]
    {:from (time/latest min-date from')
     :to (time/earliest (max-date) to')}))

(defn- parse-dates [m]
  (let [[from to] (multi-args m)
        from (parse-date from)
        to (parse-date to)]
    (restrict-dates {:from from :to to})))

(defn validate-dates [dates]
  (when (not= 2 (count dates))
    {:status 400 :body "invalid date range"}))

(defn validate-not-empty [series]
  (when (empty? series)
    {:status 400 :body "no series names specified"}))

(defn- s2i [^String s dflt]
  (try
    (Integer/parseInt s)
    (catch Exception _ dflt)))

(defn- s2d [^String s dflt]
  (try
    (Double/parseDouble s)
    (catch Exception _ dflt)))

(defn- str-true? [s]
  (= "true" (str/lower-case (or s "false"))))

(defn remove-suffix [^String s ^String suffix]
  (subs s 0 (.lastIndexOf s suffix)))

(defn mapv-pn
  ([vs] (mapv-pn vs :plant))
  ([vs key] (mapv #(update % key plant-name) vs)))


;; websocket support
#_(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket! (get-sch-adapter) {:packer (sente-transit/get-transit-packer :json
                                                                                               {:handlers tr/writers}
                                                                                               {:handlers tr/readers})
                                                     :user-id-fn (fn [req] (:client-id req))})]

  (def ring-ajax-post                ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk                       ch-recv) ; ChannelSocket's receive channel
  (def chsk-send!                    send-fn) ; ChannelSocket's send API fn
  (def connected-uids                connected-uids) ; Watchable, read-only atom
  (add-watch connected-uids :connected-uids
             (fn [_ _ old new]
               (when (not= old new)
                 (println "Connected uids change: " new)))))


;; define multiple route sets, can be composed differently if DEMO mode is enabled
(defroutes structure-routes
  (GET "/plants" [] (response (mapv-pn (get-plants) :name)))
  (GET "/structures" [names]
    (response (for [name (multi-args names) :let [s (dissoc (struc/plant-spec name) :columns)]]
                s)))
  (GET "/limits" [type]
    (response (fetch-limits type))))

(defroutes chart-routes    
  (context "/modes" [dates]
    (GET "/aggregated" []
      (or (validate-dates dates)
          (let [{:keys [from to]} (parse-dates dates)]
            (response (aggregated-modes (set (map :name (struc/plants))) from to)))))
    (GET "/individual" [plants]
      (or (validate-dates dates)
          (let [{:keys [from to]} (parse-dates dates)
                plants (mapv plant-name (multi-args plants))
                modes (db/aggregated-modes conn plants from to)]
            (response (map-values plant-name identity (db/plant-modes-stats modes)))))))       
  (POST "/data" [series start end errors? modes? changes? traces? values? only-production?] 
    (let [requested? (cond-> #{}
                       (str-true? traces?) (conj :traces)
                       (str-true? values?) (conj :values)
                       (str-true? errors?) (conj :errors)
                       (str-true? changes?) (conj :changes)
                       (str-true? modes?) (conj :modes)
                       (str-true? only-production?) (conj :only-production))
          series (map #(update % :plant plant-name) series)
          {start :from end :to} (restrict-dates {:from start :to end})]
      (response (if (empty? series) {} (fetch-chart-data series start end requested?)))))
  (GET "/errors" [plants dates]
    (or (validate-dates dates)
        (let [{:keys [from to]} (parse-dates dates)
              plants (mapv plant-name (multi-args plants))]
          (response (mapv-pn (db/errors conn plants from to))))))
  (GET "/windstats" [dates plants sector-angle]
    (let [{:keys [from to]} (parse-dates dates)
          sector-angle (s2i sector-angle 15)]
      (response (map-values plant-name identity (db/fetch-wind-stats conn (mapv plant-name (multi-args plants)) sector-angle from to)))))
  (POST "/heatmap" [start end series extents only-production?]
    (let [[{p1 :plant s1 :series :as x-series} {p2 :plant s2 :series :as y-series}] series
          p1 (plant-name p1)
          p2 (plant-name p2)           
          {:keys [x-steps y-steps min-x max-x min-y max-y]
           :or {x-steps 50 y-steps 50}} extents
          {start :from end :to} (restrict-dates {:from start :to end})
          {min-x :min max-x :max} (if (and min-x max-x)
                                    {:min min-x :max max-x}
                                    (db/query-extents conn p1 (db/series->column s1 "/avg") start end only-production?))
          {min-y :min max-y :max} (if (and min-y max-y)
                                    {:min min-y :max max-y}
                                    (db/query-extents conn p2 #_(db/series->column s1 "/std") (db/series->column s2 "/avg") start end only-production?))]
      (-> conn
          (db/heatmap-of p1 s1 p2 s2 start end only-production?
                         {:x-steps x-steps
                          :y-steps y-steps
                          :min-x min-x :max-x max-x :min-y min-y :max-y max-y})
          (assoc :x-series x-series :y-series y-series)
          response))))

(defroutes algorithm-routes
  (context "/anomalies" []
    (GET "/all-configs" [] (response (mapv-pn (db/algorithms-per-plant conn))))
    (GET "/algorithms" [plants]
      (response (for [plant (multi-args plants,)
                      {t :type :as alg} (db/algorithms-by conn {:plant (plant-name plant) :type "lasso"})]
                  (if (= t "lasso")
                    (update-in alg [:algorithm] :models) ;; we don't want to return everything (statistics etc.), just the algorithm itself
                    alg))))
    (GET "/results" [dates ids]
      (or (validate-dates dates)
          (let [{:keys [from to]} (parse-dates dates)
                ids (multi-args ids)
                ids (if (= ids ["all"]) (db/recent-algorithm-ids conn) ids)]
            (response (mapv-pn (db/algorithm-results conn ids from to))))))))

(defroutes production-routes
  (POST "/limits" [type limits]
    (if (not (struc/plant-spec type))
      {:status 400
       :body "unknown structure type"}
      (do (store-limits! type limits)
          (response ""))))
  (context "/anomalies" []      
    (POST "/model" [plant start end options]
      {:status  401
       :headers {}})))

(defroutes base-routes
  #_(GET  "/chsk" req (ring-ajax-get-or-ws-handshake req))
  #_(POST "/chsk" req (ring-ajax-post req))
  (GET "/cards" [] cards-page)
  (GET "/version" [] (version/get-version))
  (GET "/js/out/DayPicker.js.map" []  {:status 404}) ;; workaround, Chrome tries to fetch source map, parsing error then breaks Dirac Devtools
  (resources "/")
  (GET "/*" [] (loading-page)))

(defonce session-store (ring.middleware.session.memory/memory-store))

(def app
  (let [handler (-> (if demo?
                      (routes (context "/db" [] #'structure-routes #'chart-routes #'algorithm-routes) #'base-routes)
                      (routes (context "/db" [] #'structure-routes #'chart-routes #'algorithm-routes #'production-routes) #'base-routes))
                    ;(cache/wrap-control-max-age {200 (* 60 60)}) ; cache all 200 responses for one hour
                    (wrap-restful-format :formats [:transit-json :edn]
                                         :params-options {:transit-json {:handlers tr/readers}}
                                         :response-options {:transit-json {:handlers tr/writers}}
                                         ;; do not try to serialize a channel reponse
                                         ;; predicate is used for requests and responses, need to call original predicates
                                         :predicate (fn ([req] (ring.middleware.format-params/transit-json-request? req))
                                                      ([req response]
                                                       (and (not (instance? org.httpkit.server.AsyncChannel (:body response)))
                                                            (ring.middleware.format-response/serializable? req response)))))
                    wrap-cljsjs
                    ;;reuse in-memory session store, so reloading this file does not invalidate XSRF-token               
                    (wrap-defaults (assoc-in site-defaults [:session :store] session-store))                    
                    gzip/wrap-gzip)]
    (if dev?
      (-> handler wrap-exceptions wrap-reload)
      handler)))


(when (not *compile-files*)
  (defonce initialization
    (do
      ;; connection pool to database
      (def conn (let [n (env :db-name)
                          db-url (env :database-url) ; set by docker
                          spec (db/default-spec n db-url)]
                      (db/create-connection-pool! n spec)))
      
      (def task-pool (sched/mk-pool))
      ;; start periodic import of new measurement data in production
      (let [import-folder (env :import-folder)
            input-dir (env :input-folder)
            fill-cache! (fn []
                          (memo/memo-clear! aggregated-modes)
                          (aggregated-modes (set (map :name (struc/plants))) (time/date-time 2014) (time/with-time-at-start-of-day (time/now)))
                          nil)
            import-fn #(do (println "Starting file import at" (time/now) "from folder" input-dir)
                           (try
                             (let [files (etl/find-importable-files conn input-dir)]
                               (printf "found %d new files\n" (count files)) (flush)
                               (when (not-empty files)
                                 (etl/import! conn files import-folder)
                                 (println "caching aggregated modes")
                                 (fill-cache!)
                                 (println "running anomaly detections")
                                 (algo/run-algorithms! conn)                                 
                                 (println "done!")))
                             (catch Exception e
                               (.printStackTrace e))))]
        (future
          ;; initialize database metadata
          (etl/initialize-database! conn (env :db-name) (env :input-folder))
          (struc/fill-cache! (db/plant-specs conn) (db/plants conn))
          (fill-cache!)          
          (when (not demo?)
            (println "scheduling import from" input-dir "into" import-folder)
            (sched/every (time/in-millis (time/minutes 30))
                         import-fn
                         task-pool)))))))
