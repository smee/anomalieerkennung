(ns scada-ui.version
  (:require [clojure.java.io :as io]
            [environ.core :refer [env]])
  (:import java.util.Properties))

(defn get-version []
  (if-let [r (io/resource "META-INF/maven/scada-ui/scada-ui/pom.properties")]
    (let [props (doto (Properties.)
                  (.load (io/reader r)))
          version (or (env :version) (.getProperty props "version"))
          revision (or (env :revision) (.getProperty props "revision"))]
      (str version "/" revision))
    "development"))

