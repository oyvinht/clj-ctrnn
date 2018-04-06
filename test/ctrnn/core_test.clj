(ns ctrnn.core-test
  (:require [clojure.test :refer :all]
            [ctrnn.core :refer :all]
            [ctrnn.neuron :refer :all]))

(deftest test-make-ctrnn
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
   1 ; time-constant
   ))

(deftest test-make-neuron
  (testing "Make a neuron"
    (is (not (nil? (make-test-neuron))))))

(deftest test-make-neuron-makes-neuron
  (testing "Make a neuron makes a Neuron"
    (is (instance? ctrnn.neuron.Neuron (make-test-neuron)))))

(deftest test-add-synapse
  (testing "Add a synapse"
    (is (not (nil? (add-synapse
                    (make-test-neuron)
                    (make-test-neuron)
                    5))))))

(defn make-test-net []
  (let [neuron-one (make-test-neuron)
        neuron-two (make-test-neuron)]
    (add-neuron (->CTRNN [neuron-one] 0.01)
                (add-synapse neuron-one neuron-two 5))))

(deftest test-make-net
  (testing "Make a CTRNN with two neurons with a synapse between"
    (is (instance? ctrnn.core.CTRNN (make-test-net)))))

(deftest test-net-has-two-neurons
  (testing "Test CTRNN has two neurons"
    (is (= (count (neurons (make-test-net))) 2))))

(deftest test-sensible-initial-firing-frequencies
  (testing "Check calculated firing frequency of two neurons in a net"
    (let [net (make-test-net)]
      (is (every? #{0.5} (map (fn [neuron]
                                (firing-frequency neuron))
                              (neurons net)))))))

(deftest test-sensible-future-firing-frequencies
  (testing "Check calculated next membrane potential of two neurons"
    (let [net (future-ctrnn (make-test-net))]
      (doall
       (map (fn [n]
              (println (firing-frequency n)))
            (neurons net))))))
