(ns ctrnn.ctrnn
  "Simulates Continuous-Time Recurrent Neural Networks (CTRNNs)."
  {:author "oyvinht"}
  (:import ctrnn.CTRNN))

(defn make-ctrnn [size stepsize]
  (new CTRNN size stepsize))

(defn activation [ctrnn idx]
  (.getActivation ctrnn idx))

(defn set-bias [ctrnn idx bias]
  (let [new-ctrnn (.clone ctrnn)]
    (.setBias new-ctrnn idx bias)
    new-ctrnn))

(defn set-time-constant [ctrnn idx time-constant]
  (let [new-ctrnn (.clone ctrnn)]
    (.setTimeConstant new-ctrnn idx time-constant)
    new-ctrnn))

(defn set-weight [ctrnn from-idx to-idx weight]
  (let [new-ctrnn (.clone ctrnn)]
    (.setWeight new-ctrnn from-idx to-idx weight)
    new-ctrnn))

(defn time-constant [ctrnn idx]
  (.getTimeConstant ctrnn idx))

(defn update-potentials-runge-kutta [ctrnn]
  (let [new-ctrnn (.clone ctrnn)]
    (.updatePotentialsRK4 new-ctrnn)
  new-ctrnn))

(defn weight [ctrnn from-idx to-idx]
  (.getWeight ctrnn from-idx to-idx))
