(defproject ff-monitor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha1"]
                 [clj-time "0.11.0"]
                 [org.clojure/data.json "0.2.6"]
                 [com.draines/postal "1.11.3"]
                 [cprop "0.1.7"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 [org.clojure/tools.logging "0.3.1"]]
  :main ^:skip-aot ff-monitor.core
  :target-path "target/%s"
  :profiles {:dev {:jvm-opts ["-Dconf=/usr/local/etc/ff-monitor.edn"]}
             :uberjar {:aot :all}})
