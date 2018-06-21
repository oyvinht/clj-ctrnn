(defproject ctrnn "0.9.1"
  :description "Library for Continuous-Time Recurrent Neural Networks"
  :url "https://github.com/oyvinht/clj-ctrnn"
  :license
  {
   :name "MIT"
   :url "https://github.com/oyvinht/clj-ctrnn/blob/master/LICENSE"
   }
  :codox {:output-path "docs"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
