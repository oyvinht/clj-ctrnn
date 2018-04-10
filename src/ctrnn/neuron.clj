(ns ctrnn.neuron)

(defprotocol NeuronP
  (add-synapse [n from-neuron strength]
    "Add a synaptic connection from another neuron")
  (activation [n]
    "Firing frequency (activation) for current mebrane potential and bias")
  (set-bias [n b]
    "Set neuron bias")
  (set-external-current [n i]
    "Set external current")
  (update-membrane-potential [n t]
    "Approximate membrane potential for step t forward in time"))

(defn input-sum [neuron]
  (reduce (fn [in-sum synapse]
            (+ in-sum
               (* (:strength synapse)
                  (activation
                   (:from-neuron synapse)))))
          0
          (:synapses neuron)))
  
(defrecord Neuron
    [bias ; Sensivity to input
     external-current
     membrane-potential
     time-constant] ; How fast change happens (should be > than net timestep)
  NeuronP
  (add-synapse [n from-neuron strength]
    (assoc n :synapses
           (conj (:synapses n) {:from-neuron from-neuron :strength strength})))
  (activation [n]
    (/ 1 (+ 1 (java.lang.Math/exp (- (+ (:membrane-potential n) (:bias n)))))))
  (set-bias [n b]
    (assoc n :bias b))
  (set-external-current [n i]
    (assoc n :external-current i))
  (update-membrane-potential [n t]
    (assoc n :membrane-potential
           (+ (:membrane-potential n)
              (* t
                 (/ (+ (- (:membrane-potential n))
                       (:external-current n)
                       (input-sum n))
                    (:time-constant n)))))))


