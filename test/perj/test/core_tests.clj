(ns perj.test.core_tests
  (:use [perj.core])
  (:use [clojure.test]))

;(deftest replace-me ;; FIXME: write
;  (is false "No tests have been written."))

(defn simple-res [url] (transaction-record 10 url 200))

(def results
  [{:url "http://localhost:8080/article", :time 10, :status-code 200}
    {:url "http://localhost:8080/chapter", :time 20, :status-code 200}
     {:url "http://localhost:8080/chapter", :time 10, :status-code 200}
      {:url "http://localhost:8080/article", :time 20, :status-code 200}]
  )

(deftest transactions-to-url-keyed-list-test
  (let [results [(simple-res "/url/1") (simple-res "/url/1") (simple-res "/url/3") (simple-res "/url/7")]]
    (is (= (transactions-to-url-keyed-list results) [["/url/1" [10 10]] ["/url/3" [10]] ["/url/7" [10]]]))
    )
  )

(deftest remove-largest-test
  (is (= (remove-largest [5 8 6 5 3 2] 2) [5 5 3 2]))
  )

(deftest run-record-from-run-details-test
  (is (= (run-record-from-run-details 10 10000 200) {:arrival-rate 10 :throughput 20})))

;(deftest view-requests-test (view-requests results))


