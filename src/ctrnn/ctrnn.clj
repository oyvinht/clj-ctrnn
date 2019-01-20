(ns ctrnn.ctrnn
  "Simulates Continuous-Time Recurrent Neural Networks (CTRNNs)."
  {:author "oyvinht"}
  (:import [java.lang Math]))

(set! *warn-on-reflection* true)

(defn make-ctrnn [size stepsize]
  {:biases (double-array size)
   :external-currents (double-array size)
   :potentials (double-array size)
   :weights (into-array (map double-array [size size]))
   :size size
   :stepsize stepsize
   :time-constants (double-array size)})

;(defn activation [ctrnn idx]
;  (/ 1 (+ 1 (Math/exp (- (+ (aget (:potentials ctrnn) idx)
;                            (aget (:biases ctrnn) idx)))))))

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

(defn sigmoid [potential bias]
  (/ 1 (+ 1 (Math/exp (- (+ potential bias))))))

(defn update-potentials-runge-kutta [ctrnn]
  (let [biases (:biases ctrnn)
        external-currents (:external-currents ctrnn)
        potentials (:potentials ctrnn)
        time-constants (:time-constants ctrnn)
        stepsize (:stepsize ctrnn)
        weights (:weights ctrnn)
        activations (double-array (:size ctrnn))
        k1-changes (double-array (:size ctrnn))
        k2-changes (double-array (:size ctrnn))
        k3-changes (double-array (:size ctrnn))
        k4-changes (double-array (:size ctrnn))
        k-potentials (double-array (:size ctrnn))
        inputs (fn [idx]
                 (+ (aget ^doubles external-currents idx)
                    (apply + (map * activations (aget ^"[[D" weights idx)))))]
    ;; Cache current activations
    (dotimes [idx (:size ctrnn)]
      (aset-double activations idx (sigmoid (aget ^doubles potentials idx) (aget ^doubles biases idx))))
    ;; Calculate changes for k1
    (dotimes [idx (:size ctrnn)]
      (aset-double k1-changes idx
                   (* stepsize
                      (/ (- (inputs idx) (aget ^doubles potentials idx))
                         (aget ^doubles time-constants idx))))
      (aset-double k-potentials idx (+ (aget ^doubles potentials idx) (/ (aget ^doubles k1-changes idx) 2))))
    ;; Update current activations
    (dotimes [idx (:size ctrnn)]
      (aset-double activations idx (sigmoid (aget ^doubles potentials idx) (aget ^doubles biases idx))))
    ;; Calculate changes for k2
    (dotimes [idx (:size ctrnn)]
      (aset-double k1-changes idx
                   (* stepsize
                      (/ (- (inputs idx) (aget ^doubles potentials idx))
                         (aget ^doubles time-constants idx))))
      (aset-double k-potentials idx (+ (aget ^doubles potentials idx) (/ (aget ^doubles k1-changes idx) 2))))
    
    
;;      (aset k-potentials idx (+ (aget potentials idx)
     
;;      (aset k-potentials idx (+ (aget potentials idx)
;;                                (aget k4-changes idx))))
    ;; Calcuate averages
;;    (dotimes [idx (:size ctrnn)]
;;      (aset k-potentials idx (+ (aget potentials idx)
;;                                (/ (+ (aget k1-changes idx)
;;                                      (* 2 (aget k2-changes idx))
;;                                      (* 2 (aget k3-changes idx))
;;                                      (aget k4-changes idx))
;;                                   6))))
    (assoc ctrnn :potentials k-potentials)))
  
