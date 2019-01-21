(ns ctrnn.ctrnnc
  "Simulates Continuous-Time Recurrent Neural Networks (CTRNNs)."
  {:author "oyvinht"}
  (:import [java.lang Math]))

(defn make-ctrnn
  "Create CTRNN."
  [size stepsize]
  {:biases (double-array size)
   :external-currents (double-array size)
   :potentials (double-array size)
   :weights (into-array (map double-array [size size]))
   :stepsize stepsize
   :time-constants (double-array size)})

(defn activation
  "Calculate activation of neuron at idx in ctrnn."
  [ctrnn idx]
  (/ 1 (+ 1 (Math/exp (- (+ (aget ^doubles (:potentials ctrnn) idx)
                            (aget ^doubles (:biases ctrnn) idx)))))))

(defn set-bias
  "Return new CTRNN where neuron at idx has updated bias to given value."
  [ctrnn idx bias]
  (assoc ctrnn :biases
         (let [a (aclone ^doubles (:biases ctrnn))]
           (aset-double a idx bias)
           a)))

(defn set-external-current
  "Return new CTRNN where neuron at idx has external current set to given value."
  [ctrnn idx external-current]
  (assoc ctrnn :external-currents
         (let [a (aclone ^doubles (:external-currents ctrnn))]
           (aset-double a idx external-current)
           a)))

(defn set-time-constant
  "Return new CTRNN where neuron at idx has time constant set to given value."
  [ctrnn idx time-constant]
  (assoc ctrnn :time-constants
         (let [a (aclone ^doubles (:time-constants ctrnn))]
           (aset-double a idx time-constant)
           a)))

(defn set-weight
  "Return new CTRNN where neuron at idx has weight set to given value."
  [ctrnn from-idx to-idx weight]
  (assoc ctrnn :weights
         (let [a (aclone ^"[[D"(:weights ctrnn))]
           (aset-double a from-idx to-idx weight)
           a)))

(defmacro update-act!
  "Inline optimization: Destructively updates activations array in scope."
  [idx]
  `(aset
    ~'activations ~idx
    (/ 1 (+ 1 (Math/exp (- (+ (aget ~'k-potentials ~idx)
                              (aget ~'biases ~idx))))))))

(defmacro update-pot!
  "Inline optimization: Destructively update potentials array in scope."
  [^doubles k idx frac]
  `(aset
    ~'k-potentials ~idx (+ (aget ~'potentials ~idx) (/ (aget ~k ~idx) ~frac))))

(defmacro update-k!
  "Inline optimization: Destructively update k-named array in scope."
  [k idx]
  `(aset
    ~k ~idx
    (* ~'stepsize
       (/ (- (+
              (aget ~'external-currents ~idx)
              (apply + ^doubles (map * ~'activations (aget ~'weights ~idx))))
             (aget ~'k-potentials ~idx))
          (aget ~'time-constants ~idx)))))

(defn update-potentials-runge-kutta
  "Return CTRNN with potentials updated to next time step."
  [ctrnn]
  (let [^doubles biases (:biases ctrnn)
        ^doubles external-currents (:external-currents ctrnn)
        ^doubles potentials (:potentials ctrnn)
        ^doubles time-constants (:time-constants ctrnn)
        ^"[[D" weights (:weights ctrnn)
        stepsize (:stepsize ctrnn)
        size (alength potentials)
        ^doubles activations (double-array size)
        ^doubles k-potentials (aclone ^doubles potentials)
        k1 (double-array size)
        k2 (double-array size)
        k3 (double-array size)
        k4 (double-array size)]
    (dotimes [idx size] (update-act! idx)) ; Cache current activations
    (dotimes [idx size] ; Calculate changes for k1 and update activations
      (update-k! k1 idx)
      (update-pot! k1 idx 2)
      (update-act! idx))    
    (dotimes [idx size] ; Calculate changes for k2
      (update-k! k2 idx)
      (update-pot! k2 idx 2))
    (dotimes [idx size] (update-act! idx)) ; Update activations
    (dotimes [idx size] ; Calculate changes for k3
      (update-k! k3 idx)
      (update-pot! k3 idx 1))
    (dotimes [idx size] (update-act! idx)) ; Update activations
    (dotimes [idx size] ; Calculate changes for k4
      (update-k! k4 idx)
      (aset k-potentials idx (+ (aget potentials idx)
                                (/ (+ (aget k1 idx)
                                      (* 2 (aget k2 idx))
                                      (* 2 (aget k3 idx))
                                      (aget k4 idx))
                                   6))))
    (assoc ctrnn :potentials k-potentials)))
  
