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
  {:membrane-potential 0
   :time-constant 0})

(defn add-neuron
  "Returns a network with a new neuron added to it."
  [net neuron]
  (assoc :neurons (conj (net :neurons) neuron)))

(defn firing-frequency [neuron]
  "Calculate firing frequency based on current membrane potential."
  (/ 1 (+ 1 (exp (- (+ (neuron :membrane-potential) (bias neuron)))))))

(defn next-membrane-potential
  "Calculate what the membrane potential will be after timestep."
  [neuron timestep]
  (+ (* timestep
        (* (/ 1 (neuron :time-constant))
           (+ (- (neuron :membrane-potential))
              (neuron :external-current)
              (reduce (fn [in-sum syn]
                        (* (strength syn)
                           ((syn :from-neuron)
                            (firing-frequency neuron))))))))))

(defn update-membrane-potentials
  "Return a network after synchronously updating all membrane potentials."
  [net]
  (assoc net :neurons (map (fn [neuron]
                             (assoc neuron :membrane-potential
                                    (next-membrane-potential neuron timestep)))
                           (net :neurons))))

