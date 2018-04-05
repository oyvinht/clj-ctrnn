(ns ctrnn.core-test
  (:require [clojure.test :refer :all]
            [ctrnn.core :refer :all]))

(deftest test-make-ctrnn
  (testing "Make a CTRNN"
    (is (not (nil? (make-ctrnn))))))

(deftest test-make-neuron
  (testing "Make a neuron"
    (is (not (nil? (make-neuron))))))

(deftest test-make-synapse
  (testing "Make a synapse"
    (is (not (nil? (make-synapse (make-neuron)))))))

(deftest test-add-neuron
  (testing "Make a CTRNN and add a neuron"
    (is (= (count
            ((add-neuron (make-ctrnn) (make-neuron))
             :neurons))
           1))))

(deftest test-add-synapse
  (testing "Make a neuron connected to another neuron"
    (is (= (count
            ((add-synapse (make-neuron)
                          (make-synapse (make-neuron)))
             :synapses))
            1))))

(deftest test-add-synapse-in-net
  (testing "Make a CTRNN with two neurons with a synapse between"
    (let [neuron-one (make-neuron)
          neuron-two (make-neuron)
          net (add-neuron
               (add-neuron (make-ctrnn) neuron-one)
               (add-synapse neuron-two neuron-one))]
      (is (= (reduce (fn [syn-sum neuron]
                       (+ syn-sum (count (neuron :synapses))))
                     0
                     (net :neurons))
             1)))))


                
