(ns madstap.igviz.alpha-test
  (:require [madstap.igviz.alpha :as igviz]
            [clojure.test :refer [deftest is]]))

(deftest graph->selection-test
  (is (= #:igviz.selected{:nodes #{::foo ::bar},
                          :edges #{[::foo ::bar]}}
         (igviz/graph->selection #:igviz{:nodes #{#:igviz.node{:key ::foo}
                                                  #:igviz.node{:key ::bar}}
                                         :edges #{#:igviz.edge{:edge [::foo ::bar]}}}))))
