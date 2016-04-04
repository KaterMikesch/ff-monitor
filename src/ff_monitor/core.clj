(ns ff-monitor.core
  (:gen-class)
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.local :as l]
            [clojure.data.json :as json]
            [postal.core :as postal]))

;; if DEBUG true, all notification emails will be sent to a
;; test email address instead of real owners' email addresses
(def DEBUG true)

;; URLs to request for node status info
(def urls ["https://mapkoeln.kbu.freifunk.net/data/nodes.json"])

;; access paths into node status info maps
(def email-address-path [:nodeinfo :owner :contact])
(def send-alerts?-path [:nodeinfo :owner :send_alerts])
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

;; for mocking status info
(def objectpark-node-id :c46e1fe7b1c8)

;; for mocking status info
(defn mock-node-infos [nodes]
  (let [objectpark-node (objectpark-node-id nodes)
        mocked-objectpark-node (assoc objectpark-node :lastseen (t/minus (l/local-now) (t/hours 2)))]
    (assoc nodes objectpark-node-id mocked-objectpark-node)))

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
                       (node-online? x)))
          node-infos))

(defn valid-email-address? [email-address]
  (let [pattern #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"]
    (and (string? email-address) (re-matches pattern email-address))))

(defn send-notification-email
  "Assuming email-address (aka contact) in all given node-infos is the same."
  [node-infos]
  (let [email-address (get-in (first node-infos) email-address-path)
        affected-routers-text (reduce (fn [previous-text node-info]
                       (str previous-text
                            \" (get-in node-info hostname-path) \"
                            ", zuletzt gemeldet am "
                            (l/format-local-time (get-in node-info [:lastseen]) :mysql)
                            ".""\n"
                            "Zur Karte: <https://map.kbu.freifunk.net/#!v:m;n:"
                            (get-in node-info id-path) ">"                         
                            "\n\n")) "" node-infos)]
    (postal/send-message {:host "w011db5c.kasserver.com"
                          :user "m0385061"
                          :pass "kbumon"
                          :ssl true}
                         {:from "Freifunk KBU Monitor <freifunk-monitor@objectpark.org>"
                          :to (if DEBUG
                                "freifunk-monitor@objectpark.org"
                                email-address)
                          :subject "Problem mit Freifunk Router(n)?"
                          :body affected-routers-text})))

(defn -main
  "Sends notification emails to matching vanished node-owners."
  [& args]
  (let [vanished-nodes (nodes-vanished-since (node-infos (first urls))
                                             (t/minus (l/local-now) (t/hours 1)))
        nodes-for-notification (filter (fn [x]
                                         (and (send-alert-requested? x)
                                              (valid-email-address?
                                               (get-in x email-address-path))))
                                       vanished-nodes)
        grouped-by-email-address (group-by
                                  #(get-in % email-address-path)
                                  nodes-for-notification)]
    (doseq [node-infos-for-email-address grouped-by-email-address]
      (send-notification-email (nth node-infos-for-email-address 1)))
    (println "Sent" (count grouped-by-email-address) "notification email(s).")))
