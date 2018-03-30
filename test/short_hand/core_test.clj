(ns short-hand.core-test
  (:require [clojure.test :refer :all]
            [short-hand.core :refer :all]
            [clj-time.core :as t]
            [short-hand.spec :as spec]
            [clojure.spec.alpha :as s])
  (:import (clojure.lang ExceptionInfo)
           (org.joda.time DateTime)))

(deftest test-translate-short-tag-only
  (is (= (to-short-hand {:tag :hi-there}) 'hi-there))

  (is (= (to-short-hand {:tag :hi:there}) 'hi/there))
  )

(deftest test-translate-long-tag-only
  (is (= (to-long-hand 'hi-there) {:tag :hi-there}))

  (is (= (to-long-hand 'hi/there) {:tag :hi:there}))
  )

(defmethod spec/coercible :default []
  [DateTime])

(deftest test-to-long-attrs
  (is (= (to-long-hand ['a {'b 15
                            'c true
                            'd "hi"
                            'e (t/date-time 2017 5 17 22 33 45)}])
         {:tag :a :attrs {:b "15" :c "true" :d "hi" :e "2017-05-17T22:33:45.000Z"}}))
  )

(deftest test-to-short-attrs
  (is (= (to-short-hand {:tag :a :attrs {:b "15" :c "true" :d "hi"}})
         '[a {b "15" c "true" d "hi"}]))
  )

(deftest test-to-long-children

  (is (= (to-long-hand '[a b c]) {:tag :a :content [{:tag :b} {:tag :c}]}))

  (is (= (to-long-hand '[a {b true} [b {a 1}] [c "This is a string."]])
         {:tag :a :attrs {:b "true"} :content [{:tag :b :attrs {:a "1"}} {:tag :c :content ["This is a string."]}]}
         ))

  )

(deftest test-to-short-children

  (is (= (to-short-hand {:tag :a :content [{:tag :b} {:tag :c}]}) '[a b c]))

  (is (= (to-short-hand {:tag :a :attrs {:b "true"} :content [{:tag :b :attrs {:a "1"}} {:tag :c :content ["This is a string."]}]})
         '[a {b "true"} [b {a "1"}] [c "This is a string."]]))

  )

(deftest test-reserialize-attrs
  (try
    (let [long-hand {:tag :a :attrs {:b "15" :c "true" :d "hi"}}]
      (is (= long-hand (-> long-hand write-long-hand read-long-hand))))
    (catch ExceptionInfo e
      (is (= nil (.getData e)))))
  )

(deftest test-reserialize-content
  (try
    (let [long-hand {:tag :a :content [{:tag :b} {:tag :c}]}]
      (is (= long-hand (-> long-hand write-long-hand read-long-hand))))
    (catch ExceptionInfo e
      (is (= nil (.getData e)))))
  )

(deftest test-reserialize-complex
  (try
    (let [long-hand {:tag :a :attrs {:b "true"} :content [{:tag :b :attrs {:a "1"}} {:tag :c :content ["This is a string."]}]}]
      (is (= long-hand (-> long-hand write-long-hand read-long-hand))))
    (catch ExceptionInfo e
      (is (= nil (.getData e)))))
  )










