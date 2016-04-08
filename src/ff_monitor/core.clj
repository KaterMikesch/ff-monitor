(ns ff-monitor.core
  (:gen-class)
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.local :as l]
            [clostache.parser :as c]
            [clojure.data.json :as json]
            [postal.core :as postal]
            [cprop.core :refer [load-config]]))

;; if DEBUG true, all notification emails will be sent to a
;; test email address instead of real owners' email addresses
(def DEBUG true)

;; access paths into node status info maps
(def email-address-path [:nodeinfo :owner :contact])
(def send-alerts?-path [:nodeinfo :send_alerts])
(def hostname-path [:nodeinfo :hostname])
(def id-path [:nodeinfo :node_id])
(def online?-path [:flags :online])

;; convert all date info to clj-time datetime objects
(defn value-coercer [key value]
  (if (or (= key :lastseen)
          (= key :firstseen)
          (= key :timestamp))
    (f/parse value)
    value))

(def threshold-minutes 60)

(defn node-infos [url]
  (let [nodes (:nodes (json/read-str (slurp url)
                                     :key-fn keyword
                                     :value-fn value-coercer))]
    (vals (if DEBUG
            (mock-node-infos nodes)
            nodes))))

(defn send-alert-requested? [node-info]
  (get-in node-info send-alerts?-path))

(defn node-seen-since? [node-info dt]
  (t/after? (:lastseen node-info) dt))

(defn node-online? [node-info]
  (get-in node-info online?-path))

(defn nodes-vanished-since [node-infos dt]
  (filter (fn [x] (and (send-alert-requested? x)
                       (not (node-seen-since? x dt))
                       (not (node-online? x))))
          node-infos))

(defn valid-email-address?
"Checks if given string is a valid email address (RFC 2822 compliant)."
  [email-address]
  (let [pattern #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"]
    (and (string? email-address) (re-matches pattern email-address))))

(def date-formatter (f/formatter "d.M.yyyy" (t/default-time-zone)))
(def time-formatter (f/formatter "H:m"  (t/default-time-zone)))

(defn send-notification-email
  "Assuming email-address (aka contact) in all given node-infos is the same."
  [node-infos email-config]
  (let [email-address (get-in (first node-infos) email-address-path)

        affected-routers-text (reduce (fn [previous-text node-info]
                                        (let [last-seen (:lastseen node-info)
                                              replacements {:last-seen-date (f/unparse date-formatter last-seen)
                                                            :last-seen-time (f/unparse time-formatter last-seen)
                                                            :node-name  (get-in node-info hostname-path)
                                                            :node-id (get-in node-info id-path)}]
                                          (str previous-text (c/render "Router \"{{&node-name}}\", zuletzt gemeldet am {{&last-seen-date}} um {{&last-seen-time}} Uhr.\nZur Karte: <https://map.kbu.freifunk.net/#!v:m;n:{{&node-id}}>\n\n" replacements))))
                                      "" node-infos)]
    (postal/send-message (:smtp email-config)
                         {:from (:from email-config)
                          :to (if DEBUG
                                "freifunk-monitor@objectpark.org"
                                email-address)
                          :subject (:subject email-config)
                          :body (c/render (:body email-config) {:node-list affected-routers-text})})))

;; for mocking status info
(def objectpark-node1-id :c46e1fe7b1c8)
(def objectpark-node2-id :647002aac820)

;; for mocking status info
(defn mock-node-infos [nodes]
  (let [objectpark-node1 (objectpark-node1-id nodes)
        mocked-objectpark-node1 (assoc objectpark-node1 :lastseen (t/minus (l/local-now) (t/hours 2)))
        objectpark-node2 (objectpark-node2-id nodes)
        mocked-objectpark-node2 (assoc objectpark-node2 :lastseen (t/minus (l/local-now) (t/hours 2)))
        ]
    (comment (assoc nodes objectpark-node1-id mocked-objectpark-node1 objectpark-node2-id mocked-objectpark-node2))
    (println "objectpark last seen:" (:lastseen objectpark-node1) (get-in objectpark-node1 send-alerts?-path) "expired:" (node-seen-since? objectpark-node1 (t/minus (l/local-now) (t/minutes threshold-minutes))))
    nodes))

(defn -main
  "Sends notification emails to matching vanished node-owners."
  [& args]
  (let [config (load-config)
        vanished-nodes (nodes-vanished-since (node-infos (first (:nodes-urls config)))
                                             (t/minus (l/local-now) (t/minutes threshold-minutes)))
        nodes-for-notification (filter (fn [x]
                                         (and (send-alert-requested? x)
                                              (valid-email-address?
                                               (get-in x email-address-path))))
                                       vanished-nodes)
        grouped-by-email-address (group-by
                                  #(get-in % email-address-path)
                                  nodes-for-notification)]
    (doseq [node-infos-for-email-address grouped-by-email-address]
      (send-notification-email (nth node-infos-for-email-address 1) (:email config)))
    (println "Sent" (count grouped-by-email-address) "notification email(s).")))

(defn run-every-minutes [minutes f & args]
  (loop []
    (f args)
    (Thread/sleep (* 1000 60 minutes)) (recur)))
