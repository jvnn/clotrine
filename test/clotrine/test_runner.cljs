(ns clotrine.test-runner
  (:require [cljs.test :as test]
            [cljs.nodejs :as node]
            [clotrine.core-test]))

(node/enable-util-print!)

(defn run-them-tests []
  (test/run-tests 'clotrine.core-test))

(set! *main-cli-fn* run-them-tests)
