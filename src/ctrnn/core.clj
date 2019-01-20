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

(defn id [n]
  (:id n))

(defn make-neuron
  "Return new neuron with given bias and time-constant."
  [bias time-constant & {:keys [id]}]
  {:bias bias
   :id (or id (keyword (gensym 'neuron-)))
   :external-current 0
   :potential 0
   :time-constant time-constant
   :synapses []})

(defn potential [n]
  (:potential n))

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

(defn next-k-state 
  "Return a map with ctrnn and changes for each neuron."
  [orig-ctrnn k-ctrnn step-fraction]
  (reduce
   (fn [state n]
     (let [change (forward-euler-change-estimate n k-ctrnn)]
       (assoc-in
        (assoc-in state [:k-ctrnn :neurons (:id n) :potential]
                  (+ (get-in orig-ctrnn [:neurons (:id n) :potential])
                     (/ change step-fraction)))
        [:k-changes (:id n)] change)))
   {:k-ctrnn k-ctrnn}
   (neurons k-ctrnn)))

(defn update-ctrnn-runge-kutta
  "Return CTRNN with neuron membrane potentials updated to next timestep using
  4th order Runge-Kutta method."
  [ctrnn]
  (let [k1-state (next-k-state ctrnn ctrnn 2)
        k2-state (next-k-state ctrnn (:k-ctrnn k1-state) 2)
        k3-state (next-k-state ctrnn (:k-ctrnn k2-state) 1)
        k4-state (next-k-state ctrnn (:k-ctrnn k3-state) 1)]
    (assoc ctrnn :neurons
           (reduce-kv
            (fn [net neuron-id n]
              (assoc net neuron-id
                     (assoc n :potential
                            (+ (:potential n)
                               (/
                                (+ (get-in k1-state [:k-changes neuron-id])
                                   (* 2 (get-in k2-state [:k-changes neuron-id]))
                                   (* 2 (get-in k3-state [:k-changes neuron-id]))
                                   (get-in k4-state [:k-changes neuron-id]))
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

