(ns ctrnn.core
  (:require [ctrnn.neuron :as neuron]))

(defn make-ctrnn [neurons timestep]
  {:neurons neurons
   :timestep timestep})

(defn update-ctrnn [net]
  (assoc net :neurons
         (reduce-kv
          (fn [m k v]
            (assoc m k (neuron/update-membrane-potential v net)))
          {}
          (:neurons net))))
