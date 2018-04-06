(ns ctrnn.neuron)

(defprotocol NeuronP
  (add-synapse [n from-neuron strength]
    "Add a synaptic connection from another neuron")
  (firing-frequency [n]
    "Firing frequency (activation) for current mebrane potential and bias")
  (set-bias [n b]
    "Set neuron bias")
  (update-membrane-potential [n t]
    "Neuron with membrane potential at time t from current state"))

(defrecord Synapse
    [from-neuron strength])

(defrecord Neuron
    [
     bias ; Sensivity to input
     external-current
     membrane-potential
     time-constant ; How fast change happens (should be > than net timestep)
     ]
  NeuronP
  (add-synapse [n from-neuron strength]
    (assoc n :synapses
           (conj (:synapses n) (->Synapse from-neuron strength))))
  (firing-frequency [n]
    (/ 1
       (+ 1 (java.lang.Math/exp
             (- (+ (:membrane-potential n) (:bias n)))))))
  (set-bias [n b]
    (assoc n :bias b))
  (update-membrane-potential [n t]
    (assoc n :membrane-potential
           (+ (* t
                 (* (/ 1 (:time-constant n))
                    (+
                     (- (:membrane-potential n))
                     (reduce (fn [in-sum synapse]
                               (* (:strength synapse)
                                  (firing-frequency
                                   (:from-neuron synapse))))
                             0
                             (:synapses n))
                     (:external-current n))))))))
