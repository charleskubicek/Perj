(ns perj.main
   (:use [perj core]))

(perj {
      :host "http://localhost:8080"
      :duration 1000
      :rates {:steps 20
              :generators [{:name "heavy" :spec {:start 2 :step 100}}
                           {:name "light" :spec {:start 2 :step 100}}]}
      :endpoints [{:path "/article" :rate 5 :rate-generator "light"}
                  {:path "/chapter" :rate 5 :rate-generator "heavy"}]
        })

;(dbg (perj {
;             :duration 2000
;             :endpoints [{:url "http://localhost:8080/article" :rate 10}
;                         {:url "http://localhost:8080/chapter" :rate 10}]
;             }))

;(run 1000 5 "http://localhost:8080")
