(ns ctrnn.ctrnn-test
  (:require [clojure.test :refer :all]
            [ctrnn.ctrnn :refer :all]))

(defn make-test-net []
  (-> (make-ctrnn 2 0.01)
      (set-bias 1 5.0)
      (set-bias 0 -5.0)
      (set-time-constant 0 0.5)
      (set-time-constant 1 0.5)
      (set-weight 0 0 5.0)
      (set-weight 1 1 5.0)
      (set-weight 1 0 10.0)
      (set-weight 0 1 -10.0)))

(deftest t-clone-ctrnn
  (testing "Clone CTRNN."
    (let [ctrnn-2 (.clone (make-test-net))]
      (is (= (type ctrnn-2) ctrnn.CTRNN)))))


(deftest t-immutable-ctrnn
  (let [net (set-time-constant (make-test-net) 0 0.4)
        net2 (set-time-constant net 0 0.8)]
    (is (and (= (time-constant net 0) 0.4)
             (= (time-constant net2 0) 0.8)))))

(deftest t-run-runge-kutta-pulse
  (testing "Produce pulsating output from two neurons."
    ;; Set locale to format numbers the way gnuplot expect them
    (java.util.Locale/setDefault java.util.Locale/US)
    (with-open [w (clojure.java.io/writer "tex/net-output.dat")]
      (is
       (=
        (activation
         (loop [t 0 net (make-test-net)]
           (.write w
                   (str (format "%.2f " (double t))
                        (activation net 0) " "
                        (activation net 1) "\n"))
           (if (not (>= t 10))
             (recur (+ t 0.01) (update-potentials-runge-kutta net))
             net))
         0)
        0.6578399720839113)))))

(deftest t-load-runge-kutta
  (testing "Load of RK4."
    (let [start-millis (System/currentTimeMillis)]
      (is
       (=
        (activation
         (loop [t 0 net (make-test-net)]
           (if (not (>= t 10000))
             (recur (+ t 0.01) (update-potentials-runge-kutta net))
             net))
         0)
        0.7355561235763324))
      (println (str "10000 integrations of net took "
                    (- (System/currentTimeMillis) start-millis) " msecs")))))
                    
     
    
