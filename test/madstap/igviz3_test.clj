(ns madstap.igviz3-test
  (:require [madstap.igviz3 :as igviz]
            [clojure.test :refer [deftest testing is are]]))

(deftest transform-graph-test
  (let [conf {::foo {}}]
    (is (= (igviz/config->graph conf)
           (-> (igviz/config->graph conf)
               (igviz/transform-graph {}))))))
