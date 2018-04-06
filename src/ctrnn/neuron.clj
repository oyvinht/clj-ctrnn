(ns ctrnn.neuron)

(defprotocol NeuronP
  (add-synapse [n from-neuron strength]
    "Add a synaptic connection from another neuron")
  (firing-frequency [n]
    "Firing frequency with current mebrane potential and bias")
  (update-membrane-potential [n t]
    "Neuron with membrane potential at time t from current state"))

(defrecord Synapse
    [from-neuron strength])

(defrecord Neuron
    [bias external-current membrane-potential time-constant]
  NeuronP
  (add-synapse [n from-neuron strength]
    (assoc n :synapses
           (conj (:synapses n) (->Synapse from-neuron strength))))
  (firing-frequency [n]
    (/ 1
       (+ 1 (java.lang.Math/exp
             (- (+ (:membrane-potential n) (:bias n)))))))
  (update-membrane-potential [n t]
    (assoc n :membrane-potential
           (+ (* t
                 (* (/ 1 (:time-constant n))
                    (+ (- (:membrane-potential n))
                       (:external-current n)
                       (reduce (fn [in-sum synapse]
                                 (* (:strength synapse)
                                    (firing-frequency
                                     (:from-neuron synapse))))
                               0
                               (:synapses n)))))))))
