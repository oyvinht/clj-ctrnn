(ns ctrnn.core
  "Simulates Continuous-Time Recurrent Neural Networks (CTRNNs)."
  {:author "oyvinht"}
  (:import [java.lang Math]))


;;;; ---------------------------------------------------------------------------
;;;; Neurons
;;;; ---------------------------------------------------------------------------

(defn activation
  "Return current activation (firing frequency) of neuron."
  [neuron]
  (/ 1 (+ 1 (Math/exp (- (+ (:membrane-potential neuron) (:bias neuron)))))))

(defn add-synapse
  "Return neuron with new synaptic connection added."
  [neuron from-neuron strength]
  (assoc neuron :synapses
         (conj (:synapses neuron)
               {:from-neuron-id (:id from-neuron)
                :id (keyword (gensym 'synapse-))
                :strength strength})))

(defn make-neuron
  "Return new neuron with given bias and time-constant."
  [bias time-constant]
  {:bias bias
   :id (keyword (gensym 'neuron-))
   :external-current 0
   :membrane-potential 0
   :time-constant time-constant
   :synapses []})

(defn set-bias
  "Return neuron with bias set to given value."
  [neuron bias]
  (assoc neuron :bias bias))

(defn update-membrane-potential
  "Return neuron with membrane potential updated to next timestep."
  [n ctrnn]
  (let [input-sum (fn [neuron ctrnn]
                    (let [neurons (:neurons ctrnn)]
                      (reduce (fn [in-sum synapse]
                                (+ in-sum
                                   (* (:strength synapse)
                                      (activation
                                       ((:from-neuron-id synapse) neurons)))))
                              0
                              (:synapses neuron))))]
    (assoc n :membrane-potential
           (+ (:membrane-potential n)
              (* (:timestep ctrnn) ; Approximate change using forward Euler
                 (/ (+ (- (:membrane-potential n))
                       (:external-current n)
                       (input-sum n ctrnn))
                    (:time-constant n)))))))


;;;; ---------------------------------------------------------------------------
;;;; CTRNN
;;;; ---------------------------------------------------------------------------

(defn make-ctrnn
  "Return a new CTRNN with initial neurons and a set integration timestep."
  [initial-neurons timestep]
  {:neurons (reduce (fn [m n] (merge m {(:id n) n})) {} initial-neurons)
   :timestep timestep})

(defn add-neuron 
  "Return CTRNN with neuron added."
  [ctrnn neuron]
  (assoc-in ctrnn [:neurons (:id neuron)] neuron))

(defn neurons
  "Return a seq of all neurons in the CTRNN."
  [ctrnn]
  (vals (:neurons ctrnn)))

(defn update-ctrnn
  "Return CTRNN with neuron membrane potentials updated to next timestep."
  [ctrnn]
  (assoc ctrnn :neurons
         (reduce-kv
          (fn [m k v]
            (assoc m k (update-membrane-potential v ctrnn)))
          {}
          (:neurons ctrnn))))
