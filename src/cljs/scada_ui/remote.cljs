(ns scada-ui.remote
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require ajax.core
            goog.string
            [clojure.string :as str]
            [cljs.core.async :as async :refer [<! >! put! chan]]
            [taoensso.sente  :as sente :refer [cb-success?]]
            [taoensso.sente.packers.transit :as sente-transit]
            [scada-ui.util.transit :as tr]
            [scada-ui.pages.common :as pc]
            [scada-ui.format :as f]
            [cognitect.transit :as ct]
            [reagent.core :as reagent]
            [re-frame.core :refer [reg-fx dispatch]]))

(comment
  (defmulti -event-msg-handler
   "Multimethod to handle Sente `event-msg`s"
   :id                                  ; Dispatch on event-id
    )

  (defn event-msg-handler
    "Wraps `-event-msg-handler` with logging, error catching, etc."
    [{:as ev-msg :keys [id ?data event]}]
    (-event-msg-handler ev-msg))

  (defmethod -event-msg-handler
    :default ; Default/fallback case (no other matching handler)
    [{:as ev-msg :keys [event]}]
    (js/console.log (f/format "Unhandled event: %s" event)))

  (defmethod -event-msg-handler :chsk/state
    [{:as ev-msg :keys [?data]}]
    (let [[old-state-map new-state-map] ?data]
      (if (:first-open? new-state-map)
        (js/console.log (f/format "Channel socket successfully established!: %s" new-state-map))
        (js/console.log (f/format "Channel socket state change: %s"              new-state-map)))))

  (defmethod -event-msg-handler :chsk/recv
    [{:as ev-msg :keys [?data]}]
    (js/console.log (f/format "Push event from server: %s" ?data)))

  (defmethod -event-msg-handler :chsk/handshake
    [{:as ev-msg :keys [?data]}]
    (let [[?uid ?csrf-token ?handshake-data] ?data]
      (js/console.log (f/format "Handshake: %s" ?data))))

  (defn open-websocket! []
    (let [{:keys [chsk ch-recv send-fn state]}
          (sente/make-channel-socket! "/chsk"
                                      {:type :auto
                                       :packer (sente-transit/get-transit-packer :json 
                                                                                 {:handlers tr/writers}
                                                                                 {:handlers tr/readers})})]
      (js/console.log "opened new channel via websocket")
      (def chsk       chsk)
      (def ch-chsk    ch-recv) ; ChannelSocket's receive channel
      (def chsk-send! send-fn) ; ChannelSocket's send API fn
      (def chsk-state state) ; Watchable, read-only atom

      (sente/start-client-chsk-router! ch-chsk event-msg-handler)
      ))
  (defonce started! (do (open-websocket!) true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- clear-progress! [id]
  (js/setTimeout #(swap! pc/in-progress? disj id) 500))

(def req-methods {:get ajax.core/GET
                  :post ajax.core/POST})

(defn dispatch-all [vecs response]
  (cond
    (and (vector? vecs) (every? vector? vecs)) (run! #(dispatch-all % response) vecs)
    (and (vector? vecs) (keyword? (first vecs))) (dispatch (into vecs [response]))
    :else (throw (ex-info "Invalid success/error handler, should be a vector!" {:entries vecs}))))

(defn- params-to-str
"Copied from ajax.core/params-to-str, adds urlencoding to parameter values"
  [params]
  (->> (seq params)
       (mapcat (ajax.core/param-to-str nil))
       (map (fn [[k v]] (str k "=" (goog.string/urlEncode v))))
       (str/join "&")))

(defn run-request [{:keys [url method params success error hide-progress?]
                    :or {method :get}}]
  (let [id (rand)
        req-fn (get req-methods method)
        opts {:params params
              :handler (fn [resp] 
                         (try (dispatch-all success resp)
                              (finally (clear-progress! id))))
              :interceptors [(ajax.core/ProcessGet. params-to-str)]
              :error-handler (fn [resp] 
                               (try (dispatch-all error resp)
                                    (finally (clear-progress! id))))
              :format (ajax.core/transit-request-format {:writer (ct/writer :json {:handlers tr/writers})})
              :response-format (ajax.core/transit-response-format {:reader (ct/reader :json {:handlers tr/readers})})}
        opts (if (#{:post :put} method)
               (assoc-in opts [:headers "X-XSRF-TOKEN"] (.-textContent (.getElementById js/document "security-token")))
               opts)]
    (when req-fn
      (when (not hide-progress?)
        (swap! pc/in-progress? conj id))
      (req-fn url opts))))

(reg-fx :remote
        (fn [req]
          (cond
            (nil? req) ::do-nothing
            (map? req) (run-request req)
            (vector? req) (run! run-request (remove nil? req))
            :else (throw (ex-info "Invalid type of HTTP request specification!" {:request req})))))

