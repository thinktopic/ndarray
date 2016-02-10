(defproject thinktopic/ndarray "0.4.0-SNAPSHOT"
  :description  "core.matrix implementation for Clojure and Clojurescript"
  :url          "http://github/thinktopic/ndarray"
  :license      {:name "Apache Software License 2.0"
                 :url "http://www.apache.org/licenses/LICENSE-2.0"
                 :distribution :repo}
  :min-lein-vesion "2.5.0"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "0.0-3308"]
                 [thi.ng/typedarrays "0.1.2"]
                 [thi.ng/math "0.1.4"]]

  :plugins      [[lein-auto "0.1.2"]]

  :profiles     {:dev {:dependencies [[criterium "0.4.3"]]
                       :plugins [[lein-cljsbuild "1.0.6"]]
                       :global-vars {*warn-on-reflection* true}
                       :jvm-opts ^:replace []
                       :aliases {"cleantest" ["do" "clean," "test," "cljsbuild" "test"]}}}

  :auto         {:default {:file-pattern #"\.(clj|cljs|cljc)$"}}

  :cljsbuild    {:builds [{:id "simple"
                           :source-paths ["src" "test"]
                           :compiler {:output-to "target/ndarray-0.3.0.js"
                                      :optimizations :whitespace
                                      :pretty-print true}}]
                 :test-commands {"unit-tests" ["open" :runner "index.html"]}}

  :pom-addition [:developers [:developer
                              [:name "Karsten Schmidt"]
                              [:url "http://postspectacular.com"]
                              [:timezone "0"]]])
