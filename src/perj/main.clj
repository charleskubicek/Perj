(ns perj.main
   (:use [perj core]))

(perj {
      :host "http://localhost:8080"

      :load {:steps 15
             :duration 5000
             :generators [{:name "heavy" :spec {:start 2 :step 50}}
                           {:name "light" :spec {:start 2 :step 50}}]}

      :endpoints [{:path "/article" :load-generator "light"}
                  {:path "/chapter" :load-generator "heavy"}] })