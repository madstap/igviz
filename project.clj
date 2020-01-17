(defproject igviz "0.1.0"
  :description "Integrant visualization"
  :url "http://github.com/madstap/igviz"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [integrant "0.7.0" :scope "provided"]
                 [dorothy "0.0.7"]
                 [madstap/comfy "1.0.5"]]
  :profiles {:dev {:source-paths ["examples"]}})
