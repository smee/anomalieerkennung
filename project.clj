(defproject scada-ui "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "CC0 Public Domain"
            :url "n/a"}
  :repositories {"bis-snapshots" {:url "http://bisfue.uni-leipzig.de:8081/content/repositories/snapshots"
                                  :username "dokku"
                                  :password "dokku"}
                 "bis-releases" {:url "http://bisfue.uni-leipzig.de:8081/content/repositories/releases"
                                 :username "dokku"
                                 :password "dokku"}}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"] 
                 [org.clojure/clojurescript "1.9.293" :scope "provided"]
                 [org.clojure/core.async "0.2.395"]
                 [environ "1.1.0"]
                 [com.taoensso/nippy "2.12.2"] ;; speedy binary serialization
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/data.zip "0.1.2"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.clojure/core.memoize "0.5.9"]
                 [clj-time "0.13.0"] ; time/date handling server side
                 ;; server
                 [ring-server "0.4.0"] ;; TODO remove after integrating http-kit
                 [ring "1.5.0"]
                 [http-kit "2.2.0"]
                 [com.taoensso/sente "1.11.0"] ;; websockets via core.async
                 [ring/ring-defaults "0.2.1"]
                 [ring-middleware-format "0.7.0"]
                 [compojure "1.5.1"]
                 [hiccup "1.0.5"]
                 ;;[clj-http "2.1.0"] ;; for http requests, write to influxdb
                 [overtone/at-at "1.2.0"] ; simple scheduler
                 ;; DB
                 ;;[com.layerware/hugsql "0.3.0"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [mysql/mysql-connector-java "5.1.39" :upgrade false]
                 ;;[reagent "0.6.0"] ;; is required transitively by re-frame
                 [re-frame "0.9.1"]
                 ;;[day8.re-frame/async-flow-fx "0.0.6"] ;; state machine for bootup dependencies
                 ;;[day8.re-frame/forward-events-fx "0.0.5"] ;; listen to events
                 [reagent-utils "0.2.0"]
                 [ring-cljsjs "0.1.0"] ;; ring-handler for cljsjs-css resource serving
                 [cljsjs/react-day-picker "2.4.1-0"] ;; calendar
                 [cljsjs/react-bootstrap "0.30.6-0"]
                 [cljsjs/dygraph "1.1.1-1"] ; charts
                 [cljsjs/d3 "4.3.0-2"] ; for calendars, color scales, pie charts etc.
                 ;;[cljsjs/vis "4.17.0-0"] ; timelines, graph viz
                 [com.andrewmcveigh/cljs-time "0.5.0-alpha2"] ; date/time handling client side
                 [prone "1.1.4"]                      
                 [secretary "1.2.3"] ; client side routing
                 [venantius/accountant "0.1.8-SNAPSHOT" :exclusions [org.clojure/tools.analyzer.jvm]] ; automatically use client side routing when clicking on links
                 [devcards "0.2.2" :exclusions [cljsjs/react] ;; 0.2.0-8 works, 2.1.2-2 does not work in firefox. why?
                 ;; also, use react version from reagent
                  
                  ]
                 [re-frame-datatable "0.5.0"]
                 [cljs-ajax "0.5.9-SNAPSHOT"]
                 #_[keechma/forms "0.1.1" ;; form validation for reagent
                    :exclusions [[lein-doo]]] ;; lein-doo contains a public/index.html that gets delivered by figwheel when accessing the uri-path '/' instead of calling our own ring handler 
                 ;;[buddy/buddy-auth "0.8.1"]
                 [com.taoensso/tempura "1.0.0"] ;; i18n
                 [com.taoensso/encore "2.88.2"] ;; just so we use the correct transitive dependency
                 [alandipert/storage-atom "2.0.1"]
                 [io.replikativ/hasch "0.3.4"] ;; to hash nested datastructures without relying on identical string representations
                 [solar-datamining "1.2.1-SNAPSHOT"
                  :exclusions [[edu.cmu.cs/javabayes]
                               [com.github.cscheiblich/jwave]
                               [org.encog/encog-core]]]
                 [org.clojars.smee/common "1.2.9-SNAPSHOT"]
                 [chart-utils "1.1.0-SNAPSHOT"
                  :exclusions [org.jzy3d/jzy3d-api]]
                 [commons-codec/commons-codec "1.10"]]
  :plugins [[lein-environ "1.0.2"]
            [lein-cljsbuild "1.1.5"]
            [lein-asset-minifier "0.3.1"]]
  :ring {:handler scada-ui.handler/app
         :uberwar-name "scada-ui.war"}
  :min-lein-version "2.5.0"
  :uberjar-name "scada-ui.jar"
  :aliases {"dev" "repl" ;; there are problems with trampoline, don't use it!
            "build" ["do" "clean" "uberjar"]}
  :clean-targets ^{:protect false} [:target-path
                                    [:cljsbuild :builds :app :compiler :output-dir]
                                    [:cljsbuild :builds :app :compiler :output-to]]
  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]  
  :profiles {:dev {:jvm-opts ["-Xmx2g" "-XX:+UseCompressedOops" "-XX:+UseG1GC" #_"-agentpath:/tmp/liblagent.so=interval=7,logPath=/tmp/profile"] ; https://github.com/RichardWarburton/honest-profiler
                   :source-paths ["env/dev/clj" "src/cljs" "env/dev/cljs"]
                   :repl-options {:init (do
                                          (require 'scada-ui.repl)
                                          (scada-ui.repl/start-server))
                                  :nrepl-middleware [dirac.nrepl/middleware]
                                  :port 7002
                                  :host "localhost"
                                  :timeout 1200000}
                   :dependencies [[ring/ring-devel "1.5.0"]
                                  [hawk "0.2.11"] ;; figwheel uses 0.2.10
                                  [figwheel-sidecar "0.5.8"] ;; reload changed cljs
                                  [binaryage/devtools "0.8.3"] ;; format cljs datastructures, identify null dereferences
                                  [binaryage/dirac "0.8.8"] ;; with Chrome Canary, see https://github.com/binaryage/dirac/blob/master/docs/installation.md
                                  [incanter/incanter-core "1.9.1"]
                                  [incanter/incanter-charts "1.9.1"
                                   :exclusions [incanter/jfreechart]]]
                   :env {:dev "true"
                         :db-name "gt1-db"
                         :database-url "mysql://root@localhost:5029/gt1-db"
                         :input-folder "/home/steffen/daten/gt1/DATA_GTI/Downloaded_ProcessedData"
                         :import-folder "/tmp"}}
             :uberjar {:main scada-ui.server
                       :hooks [minify-assets.plugin/hooks]
                       :minify-assets {:assets {"target/cljsbuild/public/css/production.min.css"
                                                ["resources/public/css/bootstrap-paper.css"
                                                 "resources/public/css/colorbrewer.css"
                                                 "resources/public/css/site.css"]}}
                       :prep-tasks ["compile" ["cljsbuild" "once"]]
                       :env {:production "true"
                             :db-name "gt1"
                             :input-folder "/data"
                             :import-folder "/import"}                       
                       :aot :all
                       :omit-source true
                       :cljsbuild {:jar true
                                   :builds {:app
                                             {:source-paths ["src/cljs" "env/prod/cljs" ]
                                              :compiler
                                              {:output-to "target/cljsbuild/public/js/app.js"
                                               :optimizations :advanced
                                               :parallel-build true
                                               :pretty-print false
                                               :closure-defines {"goog.DEBUG" "false"}}}}}}}) 
 
