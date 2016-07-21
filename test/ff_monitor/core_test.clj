(ns ff-monitor.core-test
  (:require [clojure.test :refer :all]
            [ff-monitor.core :refer :all]
            [clj-time.core :as t]
            [clj-time.local :as l]
            [clojure.spec :as spec]))

(def interval-minutes 20)
(def node {:nodeinfo {:owner {:contact "test@example.com"}
                      :node_id "ffffffffffff"
                      :hostname "EinFreifunkKnoten"
                      :send_alerts true}
           :flags {:online false}
           :lastseen "2016-07-21T17:38:16.766Z"})

(deftest nodes-vanished-in-interval-test
  (testing "Should yield one vanished node."
    (let [test-node (assoc node :lastseen (t/minus (l/local-now) (t/minutes (+ 2 interval-minutes))))]
      (is (spec/valid? :ff-monitor.core/node test-node))
      (is (not (empty? (nodes-vanished-in-interval [test-node]
                                                   (t/minus (l/local-now) (t/minutes (* 2 interval-minutes)))
                                                   (t/minus (l/local-now) (t/minutes interval-minutes)))))))))
