(ns ctrnn.core
  "Simulates Continuous-Time Recurrent Neural Networks (CTRNNs)."
  {:author "oyvinht"}
  (:import [java.lang Math]))


;;;; ---------------------------------------------------------------------------
;;;; Neurons
;;;; ---------------------------------------------------------------------------

(defn activation
  "Return current activation (firing frequency) of neuron."
  [n]
  (/ 1 (+ 1 (Math/exp (- (+ (:potential n) (:bias n)))))))

(defn add-synapse
  "Return neuron with new synaptic connection added."
  [n from-n strength]
  (assoc n :synapses
         (conj (:synapses n)
               {:from-neuron-id (:id from-n)
                :id (keyword (gensym 'synapse-))
                :strength strength})))

(defn make-neuron
  "Return new neuron with given bias and time-constant."
  [bias time-constant & {:keys [id]}]
  {:bias bias
   :id (or id (keyword (gensym 'neuron-)))
   :external-current 0
   :potential 0
   :time-constant time-constant
   :synapses []})

(defn set-bias
  "Return neuron with bias set to given value."
  [n bias]
  (assoc n :bias bias))

(defn set-external-current
  "Return neuron with external current set to given value."
  [n external-current]
  (assoc n :external-current external-current))

(defn set-potential
  "Return copy of neuron with membrane potential set to given value."
  [n potential]
  (assoc n :potential potential))

(defn input-sum
  "Return sum of all inputs to neuron in ctrnn."
  [n ctrnn]
  (let [ns (:neurons ctrnn)]
    (reduce (fn [in-sum synapse]
              (+ in-sum
                 (* (:strength synapse)
                    (activation
                     ((:from-neuron-id synapse) ns)))))
            0
            (:synapses n))))

(defn forward-euler-change-estimate
  "Return estimated amount of change to current membrane potential."
  [n ctrnn]
  (* (:timestep ctrnn)
     (/ (+ (- (:potential n))
           (:external-current n)
           (input-sum n ctrnn))
        (:time-constant n))))
  
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
  [ctrnn n]
  (assoc-in ctrnn [:neurons (:id n)] n))

(defn neuron [ctrnn id]
  "Return neuron with given id."
  (get (:neurons ctrnn) id))

(defn add-synapses
  "Return new network with synaptic connections added."
  [ctrnn & to-from-strength-triplets]
  (loop [net ctrnn
         remain to-from-strength-triplets]
    (if (< (count remain) 3)
      net
      (let [triplet (take 3 remain)
            to-id (nth triplet 0)
            from-id (nth triplet 1)
            strength (nth triplet 2)]
        (recur (assoc-in net [:neurons to-id :synapses]
                         (conj (:synapses (neuron net to-id))
                               {:from-neuron-id from-id
                                :id (keyword (gensym 'synapse-))
                                :strength strength}))
               (seq (drop 3 remain)))))))

(defn neurons
  "Return a seq of all neurons in the CTRNN."
  [ctrnn]
  (vals (:neurons ctrnn)))

(defn update-ctrnn-forward-euler
  "Return CTRNN with neuron membrane potentials updated to next timestep using
  forward Euler method."
  [ctrnn]
  (assoc ctrnn :neurons
         (reduce-kv
          (fn [m k v]
            (assoc m k (set-potential
                        v (+ (:potential v)
                             (forward-euler-change-estimate v ctrnn)))))
          {}
          (:neurons ctrnn))))

(defn forward-euler-change-estimates
  "Return a map with neuron id as key and change estimate as value."
  [ctrnn]
  (reduce (fn [estimates n]
            (assoc estimates (:id n)
                   (forward-euler-change-estimate n ctrnn)))
          {}
          (neurons ctrnn)))

(defn update-potentials
  "Add diff to corresponding current neuron potentials in ctrnn."
  ([ctrnn diffs] (update-potentials ctrnn diffs 1))
  ([ctrnn diffs amount]   
   (assoc ctrnn :neurons
          (reduce-kv
           (fn [m k v]
             (assoc m k (set-potential
                         v (+ (:potential v)
                              (* amount ((:id v) diffs))))))
           {}
           (:neurons ctrnn)))))
          
(defn update-ctrnn-runge-kutta
  "Return CTRNN with neuron membrane potentials updated to next timestep using
  4th order Runge-Kutta method."
  [ctrnn]
  (let [k1-changes (forward-euler-change-estimates ctrnn)
        k1-ctrnn (update-potentials ctrnn k1-changes 5/10)
        k2-changes (forward-euler-change-estimates k1-ctrnn)
        k2-ctrnn (update-potentials k1-ctrnn k2-changes 5/10)
        k3-changes (forward-euler-change-estimates k2-ctrnn)
        k3-ctrnn (update-potentials k2-ctrnn k3-changes)
        k4-changes (forward-euler-change-estimates k3-ctrnn)]
    (assoc ctrnn :neurons
           (reduce-kv
            (fn [m k v]
              (assoc m k (set-potential
                          v (+ (:potential v)
                               (/ (+ ((:id v) k1-changes)
                                     (* 2 ((:id v) k2-changes))
                                     (* 2 ((:id v) k3-changes))
                                     ((:id v) k4-changes))
                                  6)))))
            {}
            (:neurons ctrnn)))))

(defn update-ctrnn
  ""
  [ctrnn]
  (update-ctrnn-forward-euler ctrnn))

(defn update-neuron
  "Return CTRNN with neuron of given id replaced by result from calling function
  with neuron as first argument + rest args."
  [ctrnn n-id function & args]
  (assoc-in ctrnn [:neurons n-id]
            (apply function (cons (neuron ctrnn n-id) args))))

