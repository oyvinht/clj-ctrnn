(ns ctrnn.ctrnn
  "Simulates Continuous-Time Recurrent Neural Networks (CTRNNs)."
  {:author "oyvinht"}
  (:import [java.lang Math]))

(defn make-ctrnn [size stepsize]
  {:biases (double-array size)
   :external-currents (double-array size)
   :potentials (double-array size)
   :weights (into-array (map double-array [size size]))
   :size size
   :stepsize stepsize
   :time-constants (double-array size)})

(defn activation [ctrnn idx]
  (/ 1 (+ 1 (Math/exp (- (+ (aget (:potentials ctrnn) idx)
                            (aget (:biases ctrnn) idx)))))))

(defn set-bias [ctrnn idx bias]
  (assoc ctrnn :biases
         (let [a (:biases ctrnn)]
           (aset-double a idx bias)
           a)))

(defn set-external-current [ctrnn idx external-current]
  (assoc ctrnn :external-currents
         (let [a (:external-currents ctrnn)]
           (aset-double a idx external-current)
           a)))

(defn set-time-constant [ctrnn idx time-constant]
  (assoc ctrnn :time-constants
         (let [a (:time-constants ctrnn)]
           (aset-double a idx time-constant)
           a)))

(defn set-weight [ctrnn from-idx to-idx weight]
  (assoc ctrnn :weights
         (let [a (:weights ctrnn)]
           (aset-double a from-idx to-idx weight)
           a)))

(defn input-sum [ctrnn idx]
  (reduce (fn [sum fidx]
            (+ sum (* (aget (:weights ctrnn) idx fidx)
                      (activation ctrnn fidx))))
          0
          (range (:size ctrnn))))

(defn update-potentials-runge-kutta [ctrnn]
  (let [external-currents (:external-currents ctrnn)
        potentials (:potentials ctrnn)
        time-constants (:time-constants ctrnn)
        stepsize (:stepsize ctrnn)
        k1-changes (double-array (:size ctrnn))
        k2-changes (double-array (:size ctrnn))
        k3-changes (double-array (:size ctrnn))
        k4-changes (double-array (:size ctrnn))
        k-potentials (double-array (:size ctrnn))]
    ;; Calculate changes for k1
    (dotimes [idx (:size ctrnn)]
      (aset k1-changes idx
            (* stepsize
               (/ (+ (- (aget potentials idx))
                     (aget external-currents idx)
                     (input-sum ctrnn idx))
                  (aget time-constants idx))))
      (aset k-potentials idx (+ (aget potentials idx)
                                (/ (aget k1-changes idx) 2))))
    ;; Calculate changes for k2
    (let [k1-net (assoc ctrnn :potentials k-potentials)]
      (dotimes [idx (:size ctrnn)]
        (aset k2-changes idx
              (* stepsize
                 (/ (+ (- (aget k-potentials idx))
                       (aget external-currents idx)
                       (input-sum k1-net idx))
                    (aget time-constants idx))))))
    (dotimes [idx (:size ctrnn)]
      (aset k-potentials idx (+ (aget potentials idx)
                                (/ (aget k2-changes idx) 2))))
    ;; Calculate changes for k3
    (let [k2-net (assoc ctrnn :potentials k-potentials)]
      (dotimes [idx (:size ctrnn)]
        (aset k3-changes idx
              (* stepsize
                 (/ (+ (- (aget k-potentials idx))
                       (aget external-currents idx)
                       (input-sum k2-net idx))
                    (aget time-constants idx))))))
    (dotimes [idx (:size ctrnn)]
       (aset k-potentials idx (+ (aget potentials idx)
                                 (aget k3-changes idx))))
    ;; Calculate changes for k4
    (let [k3-net (assoc ctrnn :potentials k-potentials)]
      (dotimes [idx (:size ctrnn)]
        (aset k4-changes idx
              (* stepsize
                 (/ (+ (- (aget k-potentials idx))
                       (aget external-currents idx)
                       (input-sum k3-net idx))
                    (aget time-constants idx))))))
    (dotimes [idx (:size ctrnn)]
      (aset k-potentials idx (+ (aget potentials idx)
                                (aget k4-changes idx))))
    ;; Calcuate averages
    (dotimes [idx (:size ctrnn)]
      (aset k-potentials idx (+ (aget potentials idx)
                                (/ (+ (aget k1-changes idx)
                                      (* 2 (aget k2-changes idx))
                                      (* 2 (aget k3-changes idx))
                                      (aget k4-changes idx))
                                   6))))
    (assoc ctrnn :potentials k-potentials)))
  
