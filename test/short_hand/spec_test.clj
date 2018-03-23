(ns short-hand.spec-test
  (:require [clojure.test :refer :all]
            [short-hand.spec :as spec]
            [clojure.spec.alpha :as s]))

(deftest test-short-name
  (is (= nil (s/explain-data ::spec/short-name 'hi-there)))

  (let [{[{in1 :in path1 :path pred1 :pred val1 :val via1 :via} problem2] ::s/problems spec ::s/spec value ::s/value} (s/explain-data ::spec/short-name '?)]
    (println (type problem1))
    (println (type problem2))
    (is (= '{:in   []
            :path [:with-ns]
            :pred (pred-i-kit.core/named-as
                    #"[a-zA-Z][a-zA-Z0-9_-]*"
                    #"[a-zA-Z][a-zA-Z0-9_-]*")
            :val  ?
            :via  [:short-hand.spec/short-name]}
           problem1))
    (is (= '{:in   []
            :path [:no-ns]
            :pred (pred-i-kit.core/named-as
                    #"[a-zA-Z][a-zA-Z0-9_-]*")
            :val  ?
            :via  [:short-hand.spec/short-name]}
           problem2))
    (is (= ::spec/short-name spec))
    (is (= '? value))
    )
  )