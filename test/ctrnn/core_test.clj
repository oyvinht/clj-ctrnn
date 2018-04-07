(ns ctrnn.core-test
  (:require [clojure.test :refer :all]
            [ctrnn.core :refer :all]
            [ctrnn.neuron :refer :all]))

(deftest make-ctrnn
  (testing "Make a CTRNN"
    (is (not (nil? (->CTRNN
                    [] ; Initial neurons
                    0.01 ; Timestep resolution (10 ms)
                    ))))))

(defn make-test-neuron []
  (->Neuron
   0 ; bias
   0 ; external-current
   0 ; initial membrane-potential
   0.5 ; time-constant
   ))

(deftest t-make-neuron
  (testing "Make a neuron"
    (is (not (nil? (make-test-neuron))))))

(deftest t-make-neuron-makes-neuron
  (testing "Make a neuron makes a Neuron"
    (is (instance? ctrnn.neuron.Neuron (make-test-neuron)))))

(deftest t-add-synapse
  (testing "Add a synapse"
    (is (not (nil? (add-synapse
                    (make-test-neuron)
                    (make-test-neuron)
                    5))))))

(defn make-test-net [bias synapse-strength]
  (let [neuron-one (make-test-neuron)
        neuron-two (set-bias (make-test-neuron) bias)]
    (add-neuron (->CTRNN [neuron-one] 0.01)
                (add-synapse neuron-one neuron-two synapse-strength))))

(deftest t-make-net
  (testing "Make a CTRNN with two neurons with a synapse between"
    (is (instance? ctrnn.core.CTRNN (make-test-net 0 0)))))

(deftest t-net-has-two-neurons
  (testing "Test CTRNN has two neurons"
    (is (= (count (neurons (make-test-net 0 0))) 2))))

(deftest t-sensible-initial-firing-frequencies
  (testing "Check calculated firing frequency of two neurons in a net"
    (let [net (make-test-net 0 0)]
      (is (every? #{0.5} (map (fn [neuron]
                                (firing-frequency neuron))
                              (neurons net)))))))

(deftest t-sensible-future-firing-frequencies-with-stronger-synapse
  (testing "Check higher firing frequency for stronger synapse"
    (let [net (future-ctrnn (make-test-net 0 1))]
      (is (some (fn [freq] (> 0.5)) (map (fn [neuron]
                                           (firing-frequency neuron))
                                         (neurons net)))))))

(deftest t-sensible-future-firing-frequencies-with-stronger-bias
  (testing "Check higher firing frequency for stronger bias"
    (let [net-one (future-ctrnn (make-test-net 0 1))
          net-two (future-ctrnn (make-test-net 1 1))]
      (is (some identity (map (fn [n-1 n-2]
                                (> (firing-frequency n-2)
                                   (firing-frequency n-1)))
                              (neurons net-one)
                              (neurons net-two)))))))

(deftest t-firing-frequency-increases-over-time
  (testing "Check if firing frequency continues to change for one more timestep"
    (let [net-one (future-ctrnn (make-test-net 0 1))
          net-two (future-ctrnn net-one)]
      (is (some identity (map (fn [n-1 n-2]
                                (> (firing-frequency n-2)
                                   (firing-frequency n-1)))
                              (neurons net-one)
                              (neurons net-two)))))))

(defn bifurcative-bias [weights]
  (/ (- (apply + weights)) 2))

(defn sqrt [num]
  (java.lang.Math/sqrt num))

(defn bifurcation-points [self-weight bias]
  (map (fn [sign]
         (- (* (sign 2)
               (java.lang.Math/log
                (/ (+ (sqrt self-weight) (sqrt (- self-weight 4))) 2)))
            (/ (sign self-weight (sqrt (* self-weight (- self-weight 4))))
               2)
            bias))
       [+ -]))

(deftest t-make-pulse-network
  (testing "A complete network that pulsates"
    ;; Make neurons
    (let [neuron-1-weights [5 1]
          neuron-2-weights [8 -2]
          neuron-3-weights [7 1]
          neuron-1 (->Neuron (bifurcative-bias neuron-1-weights) 0 0 0.2)
          neuron-2 (->Neuron (bifurcative-bias neuron-2-weights) 0 0 0.2)
          neuron-3 (->Neuron (bifurcative-bias neuron-3-weights) 0 0 0.2)]
      ;; Connect to self
      (let [neuron-1 (add-synapse neuron-1 neuron-1 (nth neuron-1-weights 0))
            neuron-2 (add-synapse neuron-2 neuron-2 (nth neuron-2-weights 0))
            neuron-3 (add-synapse neuron-3 neuron-3 (nth neuron-3-weights 0))]
        ;; Lopp and write firing frequencies to file
        (with-open [w (clojure.java.io/writer "net-output.dat")]
          (loop [t 0
                 net (->CTRNN
                      [(add-synapse neuron-1 neuron-2 (nth neuron-1-weights 1))
                       (add-synapse neuron-2 neuron-3 (nth neuron-2-weights 1))
                       (add-synapse neuron-3 neuron-1 (nth neuron-3-weights 1))]
                      0.001)]
            (let [neurons (neurons net)]
              (.write w
                      (str t " "
                           (firing-frequency (nth neurons 0)) " "
                           (firing-frequency (nth neurons 1)) " "
                           (firing-frequency (nth neurons 2)) "\n")))
            (if (not (> t 2))
              (recur (+ t 0.001) (future-ctrnn net)))))))))
       
