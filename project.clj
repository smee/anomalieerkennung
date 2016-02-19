(defproject gt1-ui "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "CC0 Public Domain"
            :url "n/a"}
  :repositories {"bis-snapshots" {:url "http://bisfue.uni-leipzig.de:8081/content/repositories/snapshots"
                                                       :username "dokku"
                                                       :password "dokku"}
                 "bis-releases" {:url "http://bisfue.uni-leipzig.de:8081/content/repositories/releases"
                                 :username "dokku"
                                 :password "dokku"}
                 "eclipse recommenders" {:url "https://repo.eclipse.org/content/repositories/recommenders/"}}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.40" :scope "provided"]
                 [environ "1.0.2"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/java.jdbc "0.6.0-alpha1"]
                 [org.clojure/core.memoize "0.5.9"]
                 [clj-time "0.11.0"] ; time/date handling server side
                 ; server
                 [ring-server "0.4.0"]
                 [ring "1.4.0"]
                 [ring/ring-defaults "0.2.0"]
                 [ring-middleware-format "0.7.0"]
                 [compojure "1.5.0"]
                 [hiccup "1.0.5"]
                 ;[clj-http "2.1.0"] ;; for http requests, write to influxdb
                 [overtone/at-at "1.2.0"] ; simple scheduler
                 ; DB
                 ;[com.layerware/hugsql "0.3.0"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [mysql/mysql-connector-java "5.1.38"]
                 ; react based ui
                 [reagent "0.6.0-alpha"]
                 [reagent-forms "0.5.22"] ; Caution on updates: contains a css file, needs to be copied to resources/css/
                 [reagent-utils "0.1.7"]
                 [historian "1.1.0"]
                 [org.clojars.frozenlock/reagent-table "0.1.3"]
                 [cljsjs/react-bootstrap "0.28.1-1"]
                 [cljsjs/dygraph "1.1.1-0"] ; charts
                 [cljsjs/d3 "3.5.7-1"] ; for calendars
                 [com.andrewmcveigh/cljs-time "0.4.0"] ; date/time handling client side
                 [prone "1.1.1"]                      
                 [secretary "1.2.3"] ; client side routing
                 [venantius/accountant "0.1.7" :exclusions [org.clojure/tools.analyzer.jvm]] ; automatically use client side routing when clicking on links
                 [devcards "0.2.1-6" :exclusions [cljsjs/react] ;; 0.2.0-8 works, 2.1.2-2 does not work in firefox. why?
                  ;; also, use react version from reagent
                  ]
                 [cljs-ajax "0.5.4"]
                 ;[com.velisco/tagged "0.4.0"] ;; enables printing/reading records as EDN
                 ;[buddy/buddy-auth "0.8.1"]
                 [alandipert/storage-atom "1.2.4"]
                 [org.eclipse.recommenders/org.eclipse.recommenders.jayes "2.3.1-SNAPSHOT"] ;; bayesian network, see http://www.codetrails.com/blog/introduction-bayesian-networks-jayes
                 [org.eclipse.recommenders/org.eclipse.recommenders.jayes.io "2.3.1-SNAPSHOT"] ;; read XMLBIF format
                 [org.apache.commons/commons-lang3 "3.4"] ;; only needed for jayes.io, not declared in their pom...
                 [solar-datamining "1.0.0-SNAPSHOT"
                  :exclusions [[edu.cmu.cs/javabayes]
                               [com.github.cscheiblich/jwave]
                               [org.encog/encog-core]]]
                 [org.clojars.smee/common "1.2.9-SNAPSHOT"]
                 [chart-utils "1.1.0-SNAPSHOT"
                  :exclusions [org.jzy3d/jzy3d-api]]]
  :plugins [[lein-environ "1.0.2"]
            [lein-cljsbuild "1.1.3"]
            [lein-asset-minifier "0.2.8"]]
  :ring {:handler gt1-ui.handler/app
         :uberwar-name "gt1-ui.war"}
  :min-lein-version "2.5.0"
  :uberjar-name "gt1-ui.jar"
  :main gt1-ui.server
  :aliases {"dev" ["trampoline" "figwheel" "app" "devcards"]
            "build" ["do" "clean" "uberjar"]}
  :clean-targets ^{:protect false} [:target-path
                                    [:cljsbuild :builds :app :compiler :output-dir]
                                    [:cljsbuild :builds :app :compiler :output-to]]
  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]
  :minify-assets {:assets {"target/cljsbuild/public/css/production.min.css"
                           ["resources/public/css/bootstrap-3.3.5.css"
                            "resources/public/css/reagent-forms.css"
                            "resources/public/css/colorbrewer.css"
                            "resources/public/css/gt1.css"
                            "resources/public/css/site.css"]
                           "target/cljsbuild/public/css/demo.min.css"
                           ["resources/public/css/bootstrap-3.3.5.css"
                            "resources/public/css/reagent-forms.css"
                            "resources/public/css/colorbrewer.css"
                            "resources/public/css/demo.css"
                            "resources/public/css/site.css"]}}
  :cljsbuild {:builds {:app {:source-paths ["src/cljs" "src/cljc"]
                             :compiler {:output-to "target/cljsbuild/public/js/app.js"
                                        :output-dir "target/cljsbuild/public/js/out"
                                        :asset-path   "js/out"
                                        :optimizations :none
                                        :parallel-build false ;; strange analysis errors if true
                                        :pretty-print  true}}}}

  :profiles {:dev {:repl-options {:init-ns gt1-ui.repl
                                  ;:init (start-server)
                                  }
                   :jvm-opts ["-Xmx4g" "-XX:+UseCompressedOops" "-XX:+UseG1GC"]
                   :dependencies [[ring/ring-mock "0.3.0"]
                                  [ring/ring-devel "1.4.0"]
                                  [lein-figwheel "0.5.2"
                                   :exclusions [[hawk]]]
                                  [hawk "0.2.10"]
                                  [org.clojure/tools.nrepl "0.2.12"]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [pjstadig/humane-test-output "0.8.0"]
                                  [binaryage/devtools "0.6.1"]
                                  [incanter/incanter-core "1.9.0"]
                                  [incanter/incanter-charts "1.9.0"
                                   :exclusions [incanter/jfreechart]]]
                   :source-paths ["env/dev/clj"]
                   :plugins [[lein-figwheel "0.5.0-6"]
                             [org.clojure/clojurescript "1.7.170"]                             
                             [com.cemerick/clojurescript.test "0.3.3"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]
                   :figwheel {:http-server-root "public"
                              :server-port 3449
                              :nrepl-port 7002
                              :ring-handler gt1-ui.handler/app
                              :nrepl-middleware ["cider.nrepl/cider-middleware"
                                                 "refactor-nrepl.middleware/wrap-refactor"
                                                 "cemerick.piggieback/wrap-cljs-repl"]
                              :css-dirs ["resources/public/css"]}
                   :env {:dev "true"
                         :db-name "gt1-db"
                         :database-url "mysql://root@localhost:5029/gt1-db"
                         :input-folder "/home/steffen/daten/gt1/DATA_GTI/Downloaded_ProcessedData"
                         :import-folder "/tmp"}
                   :cljsbuild {:builds {:app {:source-paths ["env/dev/cljs"]
                                              :compiler {:main "gt1-ui.dev"
                                                         :source-map true}}
                                        :test {:source-paths ["src/cljs" "src/cljc" "test/cljs"]
                                               :compiler {:output-to "target/test.js"
                                                          :optimizations :whitespace
                                                          :pretty-print true}}
                                        :devcards {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
                                                   :figwheel {:devcards true}
                                                   :compiler {:main "gt1-ui.cards"
                                                              :asset-path "js/devcards_out"
                                                              :output-to "target/cljsbuild/public/js/app_devcards.js"
                                                              :output-dir "target/cljsbuild/public/js/devcards_out"
                                                              :source-map-timestamp true}}}
                               :test-commands {"unit" ["phantomjs" :runner
                                                       "test/vendor/es5-shim.js"
                                                       "test/vendor/es5-sham.js"
                                                       "test/vendor/console-polyfill.js"
                                                       "target/test.js"]}}}
             :uberjar {:hooks [minify-assets.plugin/hooks]
                       :prep-tasks ["compile" ["cljsbuild" "once"]]
                       :env {:production "true"
                             :db-name "gt1"
                             :input-folder "/data"
                             :import-folder "/import"}                       
                       :aot :all
                       :omit-source true
                       :cljsbuild {:jar true
                                   :builds {:app
                                             {:source-paths ["env/prod/cljs"]
                                              :compiler
                                              {:optimizations :advanced
                                               :parallel-build false ;; strange analysis errors if true
                                               :pretty-print false}}}}}})
