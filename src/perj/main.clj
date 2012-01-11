(ns perj.main
   (:use [perj core]))

(perj {
      :host "http://localhost:8080"
      :load {:steps 15
             :duration 10000
             :generators [{:name "heavy" :spec {:start 2 :step 40}}
                           {:name "light" :spec {:start 2 :step 50}}]}
      :endpoints [{:path "/article" :load-generator "light"}
                  {:path "/chapter" :load-generator "heavy"}] })

;(dbg (perj {
;             :duration 2000
;             :endpoints [{:url "http://localhost:8080/article" :rate 10}
;                         {:url "http://localhost:8080/chapter" :rate 10}]
;             }))

;(run 1000 5 "http://localhost:8080")
