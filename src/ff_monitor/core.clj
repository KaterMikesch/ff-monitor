(ns ff-monitor.core
  (:gen-class)
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.string :as string]
            [clj-time.local :as l]
            [clostache.parser :as c]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :refer [as-url]]
            [clojure.data.json :as json]
            [postal.core :as postal]
            [postal.message :as message]
            [cprop.core :refer [load-config]]
            [clojure.spec :as spec]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp])
  (:import (java.lang Exception)
           (java.net InetAddress)))

(defn contextual-eval [ctx expr]
      (eval                                           
        `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)] 
           ~expr)))
(defmacro local-context []
          (let [symbols (keys &env)]
            (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))
(defn readr [prompt exit-code]
      (let [input (clojure.main/repl-read prompt exit-code)]
        (if (= input ::tl)
            exit-code
            input)))
;;make a break point
(defmacro break []
          `(clojure.main/repl
             :prompt #(print "debug=> ")
             :read readr
             :eval (partial contextual-eval (local-context))))

(def cli-options
     [;; First three strings describe a short-option, long-option with optional
     ;; example argument description, and a description. All three are optional
     ;; and positional.
     ["-c" "--config PATH" "Path to configuration file"
     :default "/usr/local/etc/ff-monitor.edn"
     ;    :parse-fn #(Integer/parseInt %)
     ;    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]
     ]
     ["-i" "--interval Minutes" "Number of minutes between checks"
     :default 20
     :parse-fn #(Integer/parseInt %)
     :validate [#(< 12 % 3600) "Must be a number between 0 and 3600"]]
     ["-d" "--debug" "Run only once"]
     ["-v" nil "Verbosity level; may be specified multiple times to increase value"
     ;; If no long-option is specified, an option :id must be given
     :id :verbosity
     :default 0
     ;; Use assoc-fn to create non-idempotent options
     :assoc-fn (fn [m k _] (update-in m [k] inc))]
     ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Checks for recently missing Freifunk nodes."
        ""
        "Usage: java -jar ff-monitor.jar [options]"
        ""
        "Options:"
        options-summary
        ""
        "Example:"
        "> java -jar ff-monitor.jar -i1 -c ./config.edn"
        ""]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

;; if DEBUG true, all notification emails will be sent to a
;; test email address instead of real owners' email addresses
(def DEBUG false)

(defn contains-valid-email-address?
"Checks if given string is a valid email address (RFC 2822 compliant)."
  [^String email-address]
  (let [address (message/make-address email-address "utf-8")]
    (not (nil? address))))

;; config spec
(spec/def ::url #(some? (try (as-url %) (catch Exception e))))

(spec/def ::nodes-urls (spec/* ::url))
(spec/def ::ssl boolean?)
(spec/def ::host string?)
(spec/def ::user string?)
(spec/def ::smtp (spec/keys :req-un [::host ::user] :opt-un [::pass ::ssl]))
(spec/def ::from contains-valid-email-address?)
(spec/def ::subject string?)
(spec/def ::body string?)
(spec/def ::email (spec/keys :req-un [::smtp ::from ::subject ::body]))
(spec/def ::config (spec/keys :req-un [::nodes-urls ::email]))

;; access paths into node status info maps
(spec/def ::contact (spec/or :email-address contains-valid-email-address? :rubbish string?))
(spec/def ::send_alerts boolean?)
(spec/def ::hostname string?)
(spec/def ::node_id (spec/nilable string?))
(spec/def ::online boolean?)
(spec/def ::lastseen #(instance? org.joda.time.DateTime %))

(def contact-path [:nodeinfo :owner :contact])
(def hostname-path [:nodeinfo :hostname])
(def id-path [:nodeinfo :node_id])
(def online?-path [:flags :online])

(spec/def ::flags (spec/keys :req-un [::online]))
(spec/def ::owner (spec/keys :opt-un [::contact]))
(spec/def ::nodeinfo (spec/keys :req-un [::hostname ::node_id]
                                :opt-un [::owner ::send_alerts]))
(spec/def ::node (spec/keys :req-un [::flags ::nodeinfo]
                            :opt-un [::lastseen]))
(spec/def ::nodes (spec/* ::node))

;; convert all date info to clj-time datetime objects
(defn value-coercer [key value]
  (if (or (= key :lastseen)
          (= key :firstseen)
          (= key :timestamp))
    (f/parse value)
    value))

(defn node-infos [url]
  (let [nodes (:nodes (json/read-str (slurp url)
                                            :key-fn keyword
                                            :value-fn value-coercer))]
    ;; (pp/pprint (first nodes))
    (let [node-infos (spec/conform ::nodes nodes)]
      ;; (pp/pprint node-infos)
      (if (= :clojure.spec/invalid node-infos)
        (throw (Exception. (str "Invalid nodes JSON:\n"
                                (spec/explain-str ::nodes nodes))))
        node-infos))))

(defn send-alert-requested? [node-info]
  (:send_alerts (:nodeinfo node-info)))

(defn node-online? [node-info]
  (get-in node-info online?-path))

(defn email-address [n]
  (let [contact (get-in n contact-path)]
    (if (= :email-address (first contact))
      (second contact)
      (log/warn "Illegal value for 'contact' in node:" n))))

(defn nodes-last-seen-in-interval [node-infos start-dt end-dt]
  (filter (fn [x] (and (send-alert-requested? x)
                       (t/within? start-dt end-dt (:lastseen x))
                       (not (node-online? x))))
          node-infos))

(def date-formatter (f/formatter "d.M.yyyy" (t/default-time-zone)))
(def time-formatter (f/formatter "H:m"  (t/default-time-zone)))

(defn send-notification-email
  "Assuming email-address (aka contact) in all given node-infos is the same."
  [node-infos email-config]
  (let [email-address (email-address (first node-infos))
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
                                (:from email-config)
                                email-address)
                          :subject (:subject email-config)
                          :body (c/render (:body email-config) {:node-list affected-routers-text})})))

(defn check
  "Sends notification emails to matching vanished node-owners."
  [options]
  (let [config (load-config :file (:config options))]
    (if (spec/valid? ::config config)
      (try
        (let [nodes (reduce (fn [x y]
                              (concat x (node-infos y))) [] (:nodes-urls config))
              vanished-nodes (nodes-last-seen-in-interval
                              nodes
                              (t/minus (l/local-now) (t/minutes (* 2 (:interval options))))
                              (t/minus (l/local-now) (t/minutes (:interval options))))
              nodes-for-notification (filter (fn [n]
                                               (and (send-alert-requested? n)
                                                     (email-address n)))
                                             vanished-nodes)
              grouped-by-email-address (group-by
                                        #(email-address %)
                                        nodes-for-notification)]
          (log/info "Checking"
                    (count (filter (fn [n]
                                     (and (send-alert-requested? n)
                                          (email-address n)))
                                   nodes))
                    "of" (count nodes)
                    "nodes (those which have 'send_alerts' set to true and have a valid email address in 'contact').")
          (doseq [node-infos-for-email-address grouped-by-email-address]
            (send-notification-email
             (nth node-infos-for-email-address 1) ;; nodeinfos from grouped collection
             (:email config)))
          (log/info "Sent"
                    (count grouped-by-email-address)
                    "notification email(s) for"
                    (count nodes-for-notification)
                    "vanished node(s) (using the given interval:" (:interval options) "mins)"))
        (catch Exception e (log/error e)))
      (do
        (log/error (str "Aborted. Invalid configuration file:\n"
                        (spec/explain-str ::config config)))
        (throw (Exception. (str "Aborted. Invalid configuration file:\n"
                                (spec/explain-str ::config config))))))))


(defn run-every-minutes [minutes f & args]
  (loop []
    (apply f args)
    (Thread/sleep (* 1000 60 minutes))
    (recur)))

(defn main [options]
      (let [interval (:interval options)]
        (try
          (if (:debug options)
              (check options)
              (run-every-minutes interval check options))
          (catch Exception e (System/exit 2)))))

(defn -main [& args]
      (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
        ;; Handle help and error conditions
        (cond
          (:help options) (exit 0 (usage summary))
          (.exists (clojure.java.io/as-file (:config options))) (main options)
          errors (exit 1 (error-msg errors))
          :else (exit 1 (usage summary)))))



;; future plan is having two parameters
;; - time of last check
;; - max. duration where a node is allowed to not been seen before being vanished
