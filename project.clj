(defproject clotrine "0.3.0"
  :description "A ClojureScript template rendering engine"
  :url "TODO"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.518"]]

  :plugins [[lein-npm "0.6.1"]
            [lein-cljsbuild "1.1.2"]]
  :npm
    {:dependencies
     [[source-map-support "0.4.0"]
      [markdown-it "8.1.0"]
      [sanitizer "0.1.3"]]}

  :clean-targets ^{:protect false} ["node_out" "test_out"]

  :cljsbuild
    {:builds
      {:node
        {:source-paths ["src"]
         :compiler {:main clotrine.core
                    :output-to "node_out/clotrine.js"
                    :output-dir "node_out"
                    :source-map "node_out/src-map"
                    :target :nodejs
                    :optimizations :simple
                    :verbose true}}
       :test
        {:source-paths ["src" "test"]
         :compiler {:main clotrine.test-runner
                    :output-to "test_out/test.js"
                    :output-dir "test_out"
                    :source-map true
                    :target :nodejs
                    :optimizations :none
                    :verbose true}}}})
