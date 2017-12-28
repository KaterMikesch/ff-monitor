(defproject ff-monitor "0.1.1-SNAPSHOT"
  :description "Monitoring the availabilty of Freifunk nodes."
  :url "https://github.com/KaterMikesch/ff-monitor"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-bin "0.3.4"]
            [ancient-clj "0.6.15"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clj-time "0.14.2"]
                 [org.clojure/data.json "0.2.6"]
                 [com.draines/postal "2.0.2"]
                 [cprop "0.1.11"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/tools.logging "0.4.0"]]
  :main ^:skip-aot ff-monitor.core
  :target-path "target/%s"
  :profiles {:dev {:jvm-opts ["-Dconf=/usr/local/etc/ff-monitor.edn"]}
             :bin {:name "ff-monitor"
                   :bin-path "/usr/local/bin"
                   :bootclasspath true}
             :uberjar {:aot :all}})
