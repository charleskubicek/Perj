(ns perj.core
	(:use
    [lamina core]
	  [aleph http formats]
    [incanter core io charts stats datasets]
    [clojure.contrib.math :only[expt]]
    [clojure.contrib.seq-utils :only[indexed]]
    )
  (:use
    [clojure.string :as str :exclude [replace reverse]]))

; debugging
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;
; state
;

(def completed-transactions (agent []))
(def completed-request-threads (agent 0))

;
; data objects
;

(defn transaction-record [time url status-code]
  {:url url
   :time time
   :status-code status-code})

(defn run-record [arrival-rate throughput error-rate]
  {:arrival-rate arrival-rate
   :throughput throughput
   :error-rate error-rate})


;
; math and utils
;

(defn now [] (System/currentTimeMillis))

(defn avg [l] (/ (reduce + l) (length l)))

(defn stdv [l] (let [average (avg l)]
                 (sqrt(/ (reduce + (map #(expt % 2) (map #(- % average) l))) (count l)))))

(defn response-times[requests] (map #(:time %) requests))

(defn remove-largest [c number-to-remove]
  (let [indexed (indexed c)
        indexes-to-del (set (map first (take number-to-remove (reverse (sort-by second indexed)))))]
    (map second (filter #(not (contains? indexes-to-del (first %))) indexed))))

(defn remove-outliers [c] (remove-largest c 2))

(defn remove-first-two-results [c]
  (if (odd? (count c))
    (rest (rest c))
    (rest c)))

;
; result formatting and saving
;

(defn format-requests-csv [requests]
  (map #(String/format "%s,%s,%s\n" (to-array [(:url %) (:time %) (:status-code %)]) ) requests))

(defn write-all-requests [requests]
  (let [filename "output_requests.csv"
        titles "url,response-time-ms,status-code\n"]
    (do (spit filename (apply str (join (cons titles (format-requests-csv requests))))))))

(defn save-requests-as-png [requests]
  (let [response-times (response-times requests)]
    (view (line-chart (range 0 (length response-times)) response-times ))))

(defn view-capcity-graph [results]
  (let [arrival-rate (map :arrival-rate results)
        throughput (map :throughput results)]
    (view (line-chart arrival-rate throughput))))

(defn print-summary [txs]
  (let [titles "url,response-time-avg-ms,response-time-stdv-mv"
        response-times (response-times txs)]
    (prn (apply str (join "," ["http://localhost:8080" (avg response-times) (stdv response-times)])))))

(defn make-capacity-csv [results]
  (let [filename "capacity.csv"
        header "arrival-rate,throughput,error-rate"]
      (spit filename (apply str (join "\n" (cons header
                   (map #(apply str(join "," [(:arrival-rate %) (:throughput %) (:error-rate %)] )) results)))))))

(defn key-time-pair [key time] [key time])

(defn transactions-to-url-keyed-list [txs]
  (let [grouped (group-by :url txs)]
    (map #(key-time-pair (key %) (response-times (val %))) grouped)))

(defn run-result [completed-tx url rate]
  {:txs completed-tx :url url :rate rate})

(defn run-record-from-run-result[run-result duration]
  (let [total-tx-count (reduce + (map #(count (:txs %)) (filter :txs run-result)))
        total-rate-count (reduce + (map #(:rate %) (filter :rate run-result)))
        status-codes (map :status-code (flatten (map :txs (filter :txs run-result))))
        total-error-rate (/ (count (filter #(not (= 200 %)) status-codes)) (count status-codes))]
    (prn "duration" duration)
    (prn  total-rate-count " " (double (/ total-tx-count (/ duration 1000))) " " (double total-error-rate))
    (run-record total-rate-count (double (/ total-tx-count (/ duration 1000))) (double total-error-rate))))

(defn run-records-from-run-results[run-results duration]
  (map #(run-record-from-run-result % duration) run-results))

; plots each transaction
; There is a bug in incanter: it can only draw plots if there are an even number of points on the graph.
(defn view-requests [txs]
  (let [urls (transactions-to-url-keyed-list txs)
        keys (map first urls)
        values (map remove-outliers  (map remove-first-two-results (map second urls)))
        calls-made-per-url (count (first values))
        all-calls (flatten values)
        total-calls (count all-calls)
        url-names (mapcat identity (repeat calls-made-per-url keys))
        call-index (mapcat identity (repeat (count keys) (range 0 calls-made-per-url )))
        x (flatten values) ]
    (view (line-chart call-index x :group-by url-names :legend true))))

;
; execution
;

(defn rate-for-endpoint [step endpoint generators]
  (if (nil? (:load-generator endpoint))
    (:rate endpoint)
    (let [generator-key (:load-generator endpoint)
          generator (first (filter #(= (:name %) generator-key) generators))
          gen-start (:start (:spec generator))
          gen-step (:step (:spec generator))]
      (+ gen-start (* step gen-step)))))

(defn request-callback [req url time-started agent-to-update]
  (let [request-time (- (now) time-started)]
    (send agent-to-update #(conj % (transaction-record request-time url (:status req))))
  ))

;{:status 200, :content-type "text/html; charset=iso-8859-1", :headers {"date" "Sun, 08 Jan 2012 16:33:52 GMT", "content-type" "text/html; charset=iso-8859-1", "connection" "close", "server" "Jetty(6.1.25)"}, :content-length nil, :character-encoding "iso-8859-1", :body <== []}

(defn run [time rate url]
  (let [start-time (now)
        completed-transactions (agent [])]
    (prn "running " url " with rate " rate)
    (loop []
      (if (> (+ start-time time) (now))
        (let [request-time (now)
              request (http-request {:method :get, :url url})]
          (on-success request (fn[r] (request-callback r url request-time completed-transactions)))
          (Thread/sleep (/ 1000.0 (double rate)))
          (recur)
        )
        (run-result @completed-transactions url rate)))))

(defn run-spec [spec]
  (let [host (:host spec)
        endpoints (:endpoints spec)
        rates (:load spec)
        duration (:duration rates)
        rate-generators (:generators rates)]
    (prn "perjing")
    (for [step (range 1 (+ 1 (:steps rates)))]
      (let [futures (map #(future(run duration (rate-for-endpoint step % rate-generators) (str host (:path %))  )) endpoints)]
        (loop []
          (if (not-every? #(= % true) (map #(future-done? %) futures))
            (do
              (Thread/sleep 1000)
              (recur))
            (do
              (map #(deref %) futures))
            ))))))

;
; DSL Syntax
;


;(defn run-record-from-run-details [arrival-rate duration transaction-count]
(defn perj [spec]
  (let [duration (:duration (:load spec))
        results (run-spec spec)
        run-records (run-records-from-run-results results duration)
        result (first results)]
    ;(do (prn "results: " results))
    ;(do (prn "result: " result))
    (do (view-capcity-graph run-records))
    (do (make-capacity-csv run-records))
;    (do (prn (map #(run-record-from-run-result((flatten %) duration)) results)))
;    (do (prn "result: " result))
    (do (Thread/sleep 500)) ; wait a bit...
    (do (prn "** COMPLETED ***"))
  ;  (do (example))
    ;(do (view-requests (flatten result)))
    ;(do (write-all-requests result))
    ;(do (print-summary result ))
 ;   (do (save-requests-as-png @completed-transactions))
  ))
