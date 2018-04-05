(defproject ctrnn "0.1.0-SNAPSHOT"
  :description "Library for Continuous-Time Recurrent Neural Networks"
  :url "https://github.com/oyvinht/clj-ctrnn"
  :license
  {
   :name "MIT"
   :url "https://github.com/oyvinht/clj-ctrnn/blob/master/LICENSE"
   }
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot ctrnn.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
