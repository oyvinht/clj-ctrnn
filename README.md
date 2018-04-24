# clj-ctrnn
Clojure library for simulating Continuous-Time Recurrent Neural Networks

## Installation

[![Clojars Project](https://img.shields.io/clojars/v/ctrnn.svg)](https://clojars.org/ctrnn)

## Usage
See the tests for more complete examples.

Make a CTRNN and print activation of one of the neurons for 10 timesteps:
```clojure
<<<<<<< HEAD
(let [neuron-1 (net/make-neuron -5 0.5)
      neuron-2 (net/add-synapse (net/make-neuron 1 0.5) neuron-1 -20)]
        neuron-2 (net/add-synapse (net/make-neuron 1 0.5) neuron-1 -20)]
        neuron-2 (net/add-synapse (net/make-neuron 1 0.5) neuron-1 -20)]
=======
(let [neuron-1 (make-neuron -5 0.5)
      neuron-2 (add-synapse (make-neuron 1 0.5) neuron-1 -20)]
>>>>>>> 39f3ebb... Adding slightly better usage example.
    (loop [t 0
           net (make-ctrnn [neuron-1 neuron-2] 0.01)]
      (println (activation ((:id neuron-2) (:neurons net))))
      (if (< t 0.1)
        (recur (+ t 0.01) (update-ctrnn net))))))
```

## Documentation
Generated codox documentation is [available here](https://oyvinht.github.io/clj-ctrnn/docs/index.html "Codox"). Mathematical background is [outlined here](https://github.com/oyvinht/clj-ctrnn/blob/master/tex/maths.pdf "Mathematical Background for CTRNN Simulations").
