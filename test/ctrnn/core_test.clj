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
   0 ; external current
   0 ; initial membrane potential
   0.5)) ; time-constant

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

(deftest t-sensible-initial-activation
  (testing "Check calculated activation of two neurons in a net"
    (let [net (make-test-net 0 0)]
      (is (every? #{0.5} (map (fn [neuron]
                                (activation neuron))
                              (neurons net)))))))

(deftest t-sensible-future-activation-with-stronger-synapse
  (testing "Check higher activation for stronger synapse"
    (let [net (future-ctrnn (make-test-net 0 1))]
      (is (some (fn [freq] (> 0.5)) (map (fn [neuron]
                                           (activation neuron))
                                         (neurons net)))))))

(deftest t-sensible-future-activation-with-stronger-bias
  (testing "Check higher activation for stronger bias"
    (let [net-one (future-ctrnn (make-test-net 0 1))
          net-two (future-ctrnn (make-test-net 1 1))]
      (is (some identity (map (fn [n-1 n-2]
                                (> (activation n-2)
                                   (activation n-1)))
                              (neurons net-one)
                              (neurons net-two)))))))

(deftest t-activation-increases-over-time
  (testing "Check if activation continues to change for one more timestep"
    (let [net-one (future-ctrnn (make-test-net 0 1))
          net-two (future-ctrnn net-one)]
      (is (some identity (map (fn [n-1 n-2]
                                (> (activation n-2)
                                   (activation n-1)))
                              (neurons net-one)
                              (neurons net-two)))))))

(deftest test-single-neuron-net
  (testing "Membrane potential of neuron in single neuron net"
    (is
     (every?
      identity
      (map = (list 0.0 3.3E-4 6.7E-4 9.9E-4 0.00132 0.00164)
           ;; Make neuron
           (let [neuron-1 (->Neuron -5 0 0 1)]
             ;; Connect to self
             (let [neuron-1 (add-synapse neuron-1 neuron-1 5)]
               ;; Loop, collecting membrane potential reading
               (loop [t 0
                      net (->CTRNN [neuron-1] 0.01)
                      potentials []]
                 (let [neurons (neurons net)]
                   (if (> t 0.05)
                     (map (fn [val]
                            (/ (java.lang.Math/round (* val 1.0E5)) 1.0E5))
                          potentials)
                     (recur (+ t 0.01)
                            (future-ctrnn net)
                            (conj potentials
                                  (:membrane-potential (first neurons))))))))))))))

(deftest t-make-pulse-network
  (testing "A complete network that pulsates"
    ;; Make neurons
    (let [neuron-1 (->Neuron 0 0 0 0.45)
          neuron-2 (->Neuron 0 0 0 0.45)]
      ;; Connect to self
      (let [neuron-1 neuron-1 ;(add-synapse neuron-1 neuron-1 5)
            neuron-2 neuron-2 ];(add-synapse neuron-2 neuron-2 5)]
        ;; Loop and write activations to file
        (with-open [w (clojure.java.io/writer "net-output.dat")]
          (loop [t 0
                 net (->CTRNN
                      [neuron-1 neuron-2]
;                      [(add-synapse neuron-1 neuron-2 5)
;                       (add-synapse neuron-2 neuron-1 5)]
                      0.01)]
            (let [neurons (neurons net)]
              (.write w
                      (str t " "
                           (activation (nth neurons 0)) " "
                           (activation (nth neurons 1)) " "
                           (:membrane-potential (nth neurons 0)) " "
                           (:membrane-potential (nth neurons 1)) "\n"
                           )))
            (if (not (> t 0.02))
              (recur (+ t 0.01) (future-ctrnn net)))))))))
       
