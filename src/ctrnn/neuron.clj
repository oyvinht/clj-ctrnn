(ns ctrnn.neuron)

(defn sigmoid [x]
  (/ 1 (+ 1 (java.lang.Math/exp (- x)))))

(defn activation [n]
  (sigmoid (+ (:membrane-potential n) (:bias n))))

(defn input-sum [neuron net]
  (let [neurons (:neurons net)]
    (reduce (fn [in-sum synapse]
              (+ in-sum
                 (* (:strength synapse)
                    (activation ((keyword (:from-neuron-id synapse))
                                 neurons)))))
            0
            (:synapses neuron))))

(defn make-neuron [id bias time-constant]
  {:bias bias
   :id id
   :external-current 0
   :membrane-potential 0
   :time-constant time-constant
   :synapses []})

(defn add-synapse [neuron from-neuron-id strength]
  (assoc neuron :synapses
         (conj (:synapses neuron)
               {:from-neuron-id from-neuron-id
                :strength strength})))

(defn update-membrane-potential [n net]
  (assoc n :membrane-potential
         (+ (:membrane-potential n)
            (* (:timestep net)
               (/ (+ (- (:membrane-potential n))
                     (:external-current n)
                     (input-sum n net))
                  (:time-constant n))))))



