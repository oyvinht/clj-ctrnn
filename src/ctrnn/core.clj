(ns ctrnn.core
  (:require [ctrnn.neuron :as neuron]))

(defprotocol CTRNNP
  (add-neuron [net ^ctrnn.neuron.Neuron neuron])
  (future-ctrnn [net]
    "Synchronously update all neurons one timestep")
  (^clojure.lang.PersistentVector neurons [net]))

(defrecord CTRNN
    [;; Initial list of neurons
     neurons
     ;; Resolution (e.g. number of milliseconds per timestep) that should
     ;; always be kept smaller than any time-constants set in neurons
     ^Double timestep
     ]
  CTRNNP
  (add-neuron [net neuron]
    (assoc net :neurons (conj (:neurons net) neuron)))
  (future-ctrnn [net]
    (assoc net :neurons
           (map (fn [n]
                  (neuron/update-membrane-potential n (:timestep net)))
                (:neurons net))))
  (neurons [net] (:neurons net)))
