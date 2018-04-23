(ns ctrnn.core-test
  (:require [clojure.test :refer :all]
            [ctrnn.core :refer :all]))

(defn make-test-neuron []
  (make-neuron
   0 ; bias
   0.5)) ; time-constant

(defn bifurcation-points [w b]
  (map (fn [sign]
         (-
          (* (sign 2)
             (java.lang.Math/log (/ (+ (java.lang.Math/sqrt w)
                                       (java.lang.Math/sqrt (- w 4)))
                                    2)))
          (/ (sign w (java.lang.Math/sqrt (* w (- w 4))))
             2)
          b))
       (list + -)))

(deftest t-make-neuron
  (testing "Make a neuron"
    (is (not (nil? (make-test-neuron))))))

(deftest t-add-synapse
  (testing "Add a synapse"
    (is (not (nil? (add-synapse
                    (make-test-neuron)
                    (make-test-neuron)
                    5))))))

(defn make-test-net [bias synapse-strength]
  (let [neuron-one (make-test-neuron)
        neuron-two (set-bias (make-test-neuron) bias)]
    (add-neuron (make-ctrnn [neuron-one] 0.01)
                (add-synapse neuron-two neuron-one synapse-strength))))

(deftest t-net-has-two-neurons
  (testing "Test CTRNN has two neurons"
    (is (= (count (neurons (make-test-net 0 0))) 2))))

(deftest t-sensible-initial-activation
  (testing "Check calculated activation of two neurons in a net"
    (let [net (make-test-net 0 0)]
      (is (every? #{0.5} (for [[id neuron] (:neurons net)]
                           (activation neuron)))))))

(deftest t-sensible-update-activation-with-stronger-synapse
  (testing "Check higher activation for stronger synapse"
    (let [net (update-ctrnn (make-test-net 0 1))]
      (is (some (fn [freq] (> 0.5)) (map (fn [neuron]
                                           (activation neuron))
                                         (neurons net)))))))

(deftest t-sensible-update-activation-with-stronger-bias
  (testing "Check higher activation for stronger bias"
    (let [net-one (update-ctrnn (make-test-net 0 1))
          net-two (update-ctrnn (make-test-net 1 1))]
      (is (some identity (map (fn [n-1 n-2]
                                (> (activation n-2)
                                   (activation n-1)))
                              (neurons net-one)
                              (neurons net-two)))))))

(deftest t-activation-increases-over-time
  (testing "Check if activation continues to change for one more timestep"
    (let [net-one (update-ctrnn  (make-test-net 0 1))
         net-two (update-ctrnn net-one)]
      (is (some identity (map (fn [n-1 n-2]
                                (> (activation n-2)
                                   (activation n-1)))
                              (neurons net-one)
                              (neurons net-two)))))))

(deftest test-single-neuron-net
  (testing "Expected membrane potential of neuron in single neuron net"
    (is
     (every?
      identity
      (map = (list 0.0 3.3E-4 6.7E-4 9.9E-4 0.00132 0.00164)
           ;; Make neuron
           (let [neuron-1 (make-neuron -5 1)]
             
             (let [neuron-1 (add-synapse neuron-1 neuron-1 5)]
               ;; Loop, collecting membrane potential reading
               (loop [t 0
                      net (make-ctrnn [neuron-1] 0.01)
                      potentials []]
                 (let [neurons (neurons net)]
                   (if (> t 0.05)
                     (map (fn [val]
                            (/ (java.lang.Math/round (* val 1.0E5)) 1.0E5))
                          potentials)
                     (recur (+ t 0.01)
                            (update-ctrnn net)
                            (conj potentials
                                  (:membrane-potential
                                   (first neurons))))))))))))))

