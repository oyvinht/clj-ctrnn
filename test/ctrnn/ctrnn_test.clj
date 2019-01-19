(ns ctrnn.ctrnn-test
  (:require [clojure.test :refer :all]
            [ctrnn.ctrnn :refer :all]))

(defn make-test-net []
  (doto (make-ctrnn 2 0.01)
    (set-bias 0 -5)
    (set-bias 1 5)
    (set-time-constant 0 0.5)
    (set-time-constant 1 0.5)
    (set-weight 0 0 5)
    (set-weight 1 1 5)
    (set-weight 0 1 10)
    (set-weight 1 0 -10)))
      
(defn t-sensible-activation []
  (is (= (activation (make-test-net) 0)
         0.0066928509242848554)))
           
(defn t-check-change-estimate []
  (let [net (make-test-net)]
    (forward-euler-change-estimate net 0)))

(defn t-load-runge-kutta []
  (let [start-millis (System/currentTimeMillis)]
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
        0.7355561235763324)))
    (println (- (System/currentTimeMillis) start-millis))))
     
    
