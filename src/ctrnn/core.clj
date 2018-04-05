(ns ctrnn.core
  (:gen-class))

(defn -main
  ""
  [& args])

(defn make-ctrnn []
  {
   :neurons []
   }
  )

(defn make-neuron []
  {:bias 0
   :synapses []
   :membrane-potential 0
   :time-constant 0})

(defn make-synapse [from-neuron]
  {:from-neuron from-neuron
   :strength 0
   })
  
(defn add-neuron
  "Returns a network with a new neuron added to it."
  [net neuron]
  (assoc net :neurons (conj (net :neurons) neuron)))

(defn add-synapse
  "Returns a neuron with a new synaptic connection added"
  [neuron synapse]
  (assoc neuron :synapses (conj (neuron :synapses) synapse)))

(defn firing-frequency [neuron]
  "Calculate firing frequency based on current membrane potential."
  (/ 1 (+ 1 (java.lang.Math/exp
             (- (+ (neuron :membrane-potential) (neuron :bias)))))))

(defn next-membrane-potential
  "Calculate what the membrane potential will be after timestep."
  [neuron timestep]
  (+ (* timestep
        (* (/ 1 (neuron :time-constant))
           (+ (- (neuron :membrane-potential))
              (neuron :external-current)
              (reduce (fn [in-sum syn]
                        (* (syn :strength)
                           ((syn :from-neuron)
                            (firing-frequency neuron))))))))))

(defn update-membrane-potentials
  "Return a net after synchronously updating all membrane potentials."
  [net timestep]
  (assoc net :neurons
         (map (fn [neuron]
                (assoc neuron :membrane-potential
                       (next-membrane-potential neuron timestep)))
              (net :neurons))))

