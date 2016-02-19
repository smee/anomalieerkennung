(ns gt1-ui.bayes
  (:require [clojure.java.io :as io])
  (:import [org.eclipse.recommenders.jayes BayesNet BayesNode]
           org.eclipse.recommenders.jayes.io.xmlbif.XMLBIFReader
           org.eclipse.recommenders.jayes.inference.jtree.JunctionTreeAlgorithm))


(defn- node [^BayesNet net name outcomes]
  {:pre [(every? string? outcomes) (sequential? outcomes)]}
  (doto (.createNode net name)
    (.addOutcomes (into-array String outcomes))))

;; TODO respect ratio of #highly-correlated in divergent models/#as-predictor overall
(def sensor-network-structure
  {:nodes {"exists as predictor" ["no" "few" "many"]
           "suspicious candidate in models" ["no" "few" "many" "all"]
           "target divergent" ["yes" "no"]
           "correlated predictors" ["none" "few" "many"]
           "has model" ["yes" "no"]
           "predictor suspicious" ["yes" "no"]
           "target candidate" ["yes" "no"]
           "is constant" ["yes" "no"]
           "is reason" ["yes" "no"]}
   :parents {"predictor suspicious" ["exists as predictor" "suspicious candidate in models"]
             "target candidate" ["correlated predictors" "target divergent" "is constant"]
             "is reason" ["predictor suspicious" "target candidate" "has model"]}
   :probabilities {"exists as predictor" [0.02 0.08 0.90]
                   "suspicious candidate in models" [0.9 0.06 0.03 0.01]
                   "target divergent" [0.02 0.98]
                   "correlated predictors" [0.2 0.5 0.3]
                   "has model" [0.67 0.33]
                   "predictor suspicious" [;;yes no   ex. pred. over thres. 
                                           0.0  1.0  ;; no         no
                                           0.0  1.0  ;; no         few
                                           0.0  1.0  ;; no         many
                                           0.0  1.0  ;; no         all 
                                           0.0  1.0  ;; few        no  
                                           0.6  0.4  ;; few        few 
                                           0.8  0.2  ;; few        many
                                           0.9  0.1  ;; few        all 
                                           0.0  1.0  ;; many       no  
                                           0.8  0.2  ;; many       few 
                                           0.9  0.1  ;; many       many
                                           0.99 0.01 ;; many       all 
                                           ]
                   "target candidate" [;;yes no    corr. divrg. constant?
                                       0.99 0.01 ;; none  yes    yes
                                       0.99 0.01 ;; none  yes    no
                                       0.01 0.99 ;; none  no     yes
                                       0.01 0.99 ;; none  no     no
                                       0.8  0.2  ;; few   yes    yes
                                       0.2  0.8  ;; few   yes    no
                                       0.1  0.9  ;; few   no     yes
                                       0.01 0.99 ;; few   no     no
                                       0.9  0.1  ;; many  yes    yes
                                       0.8  0.2  ;; many  yes    no
                                       0.6  0.4  ;; many  no     yes
                                       0.0  1.0  ;; many  no     no
                                       ]
                   "is reason" [;; yes no   pred. target has model
                                1.0  0.0  ;; yes   yes    yes
                                0.9  0.1  ;; yes   yes    no
                                0.7  0.3  ;; yes   no     yes
                                0.9  0.1  ;; yes   no     no
                                0.8  0.2  ;; no    yes    yes
                                0.0  1.0  ;; no    yes    no
                                0.0  1.0  ;; no    no     yes
                                0.0  1.0  ;; no    no     no
                                ]}})

(defn read-net [resource]
  (->> "lasso-root-cause-v5.xmlbif"
       io/resource
       io/input-stream
       XMLBIFReader.
       .read))

(defn bayesian-network [{:keys [nodes parents probabilities]}]
  "The order of probabilities is determined by the order of outcomes per parent,
comparable to the order of a nested for-loop."
  (let [net (BayesNet.)
        nodes (reduce (fn [m [name outcomes]]
                        (assoc m name {:node (node net name outcomes)
                                       :outcomes outcomes}))
                      {} nodes)]
    ;; set parents and probabilities
    (run! (fn [[name {^BayesNode node :node oc :outcomes}]]
            (let [n (count oc)
                  default-prob (vec (repeat n (/ 1.0 n)))
                  parent-names (get parents name)
                  parent-nodes (mapv #(:node (get nodes %)) parent-names)] 
              (when parent-names
                (.setParents node parent-nodes))
              (.setProbabilities node (double-array (get probabilities name default-prob)))))
          nodes)
    net))

(defn calc [^BayesNet net ^BayesNode target-node-name evidence]
  (let [evidence (reduce-kv (fn [m node-name value]
                              (assoc m (.getNode net ^String node-name) value))
                            {} evidence)
        target-node (.getNode net ^String target-node-name)
        inferrer (doto (JunctionTreeAlgorithm.)
                   (.setNetwork net)
                   (.setEvidence evidence))]
    (zipmap (.getOutcomes target-node)
            (.getBeliefs inferrer target-node))))

(defn load-net []
  (read-net "lasso-root-cause-v5.xmlbif"))

(comment

  (def net (bayesian-network sensor-network-structure))
  (calc net "is reason" {"suspicious candidate in models" "many"
                         "exists as predictor" "few"
                         "target divergent" "no"
                         "correlated predictors" "few"
                         "has model" "yes"
                         "is constant" "no"})

  (calc (load-net) "is_reason"
        {"suspicious_candidate" "many"
         "exists_as_predictor" "few"
         "target_divergent" "no"
         "corr_predictors" "few"
         "has_model" "yes"
         "is_constant" "no"})

  )
