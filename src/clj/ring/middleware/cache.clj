(ns ring.middleware.cache
  (:require [ring.util.response :as res]))

(defn- has-cache-directives? [{hdrs :headers}]
  (or
   (get hdrs "Cache-Control")
   (get hdrs "Expires")))

(defn- cache-control-max-age-handler [handler get-status-age]
  "Excutes the handler. The second argument is a function
   that accepts an HTTP status code (Integer) and returns
   max-age in seconds or nil."
  (fn [request]
    (let [response (handler request)
          status (get response :status 0)
          age (get-status-age status)]
      (if (or (has-cache-directives? response) (not age))
        response
        (res/header response "Cache-Control" (format "max-age=%d" age))))))

(defn wrap-control-max-age [handler status-age]
  "Middleware excutes the handler and if the response does not
   have any Cache-Control or Expires directives, and the status code
   exists in the given status-age map, then a 'Cache-Control: max-age'
   header is added to the response with the duration given in the map.
   Second parameter is a map of HTTP status codes to age in seconds
   e.g. {200 60, 404 3600}"
  (cache-control-max-age-handler
    handler
    (partial get status-age)))
