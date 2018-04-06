(ns ctrnn.core
  (:require [ctrnn.neuron :as neuron])
  (:gen-class))

(defn -main
  ""
  [& args])

(defprotocol CTRNNP
  (add-neuron [net ^ctrnn.neuron.Neuron neuron])
  (future-ctrnn [net t])
  (neurons [net]))

(defrecord CTRNN
    [neurons]
  CTRNNP
  (add-neuron [net neuron]
    (assoc net :neurons (conj (:neurons net) neuron)))
  (future-ctrnn [net t]
    "The net at time t (after synchronously updating all neurons)"
    (assoc net :neurons
           (map (fn [n]
                  (neuron/update-membrane-potential n t))
                (:neurons net))))
  (neurons [net] (:neurons net)))

