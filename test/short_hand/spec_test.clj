(ns short-hand.spec-test
  (:require [clojure.test :refer :all]
            [short-hand.spec :as spec]
            [clojure.spec.alpha :as s]
            [clj-time.core :as t]
            [clojure.java.io :as io])
  (:import (java.net URL URI)
           (java.io File)
           (org.joda.time DateTime)
           (java.nio.file Path)))

(deftest test-short-name
  (is (= nil (s/explain-data ::spec/short-name :hi-there)))

  (is (= nil (s/explain-data ::spec/short-name :hi/there)))

  (let [{problems ::s/problems spec ::s/spec value ::s/value} (s/explain-data ::spec/short-name :?)
        [{in1 :in path1 :path [func1 ns-reg name-reg] :pred val1 :val via1 :via}
         {in2 :in path2 :path [func2 regex] :pred val2 :val via2 :via}]
        problems]
    (is (= (count problems) 2))
    (is (= [] in1))
    (is (= 'pred-i-kit.core/named-as func1))
    (is (= (str #"[a-zA-Z][a-zA-Z0-9_-]*") (str ns-reg)))
    (is (= (str #"[a-zA-Z][a-zA-Z0-9_-]*") (str name-reg)))
    (is (= [:with-ns] path1))
    (is (= :? val1))
    (is (= [::spec/short-name] via1))
    (is (= [] in2))
    (is (= 'pred-i-kit.core/named-as func2))
    (is (= (str #"[a-zA-Z][a-zA-Z0-9_-]*") (str regex)))
    (is (= [:no-ns] path2))
    (is (= :? val2))
    (is (= [::spec/short-name] via2))
    (is (= ::spec/short-name spec))
    (is (= :? value))
    )
  )

(deftest test-long-tag
  (is (= nil (s/explain-data ::spec/tag :hi-there)))

  (is (= nil (s/explain-data ::spec/tag :hi:there)))

  (let [{problems ::s/problems spec ::s/spec value ::s/value} (s/explain-data ::spec/tag :?)
        [{in2 :in path2 :path [func2 regex] :pred val2 :val via2 :via}]
        problems]
    (is (= (count problems) 1))
    (is (= [] in2))
    (is (= 'pred-i-kit.core/named-as func2))
    (is (= (str #"[a-zA-Z][a-zA-Z0-9_-]*([:][a-zA-Z][a-zA-Z0-9_-]*)?") (str regex)))
    (is (= [] path2))
    (is (= :? val2))
    (is (= [::spec/tag] via2))
    (is (= ::spec/tag spec))
    (is (= :? value))
    )
  )

(deftest test-long-node-tag-only
  (is (= nil (s/explain-data ::spec/long-node {:tag :hi-there})))

  (is (= nil (s/explain-data ::spec/long-node {:tag :hi:there})))

  )

(defmethod spec/coercible :default []
  [DateTime File URL URI Path]) ;remove classes from list to see failures on test below

(deftest test-primitive
  (is (= nil (s/explain-data ::spec/short-primitive (t/date-time 2014 5 17 22 33 45))))
  (is (= nil (s/explain-data ::spec/short-primitive (io/file "hi-there.txt"))))
  (is (= nil (s/explain-data ::spec/short-primitive (URL. "http://www.imdb.com"))))
  (is (= nil (s/explain-data ::spec/short-primitive (URI. "hi-there.txt"))))
  (is (= nil (s/explain-data ::spec/short-primitive (.toPath ^File (io/file "hi-there.txt")))))
  )

(deftest test-attrs
  (is (= nil (s/explain-data
               ::spec/short-attrs
               {:a true
                :b 324
                :c 4.5
                :d 7/22
                :e 22/7
                :f "hi there"
                :g (t/date-time 2014 5 17 22 33 45)
                :h (io/file "hi-there.txt")
                :i (URL. "http://www.imdb.com")
                :j (URI. "hi-there.txt")
                :k (.toPath ^File (io/file "hi-there.txt"))})))
  )

(deftest test-long-attr-str
  (is (= nil (s/explain-data ::spec/attrs {:a "hi there"}))))

(deftest test-long-attr-bool
  (is (= {::s/problems (list {:in [:a 1]
                              :path [1]
                              :pred 'clojure.core/string?
                              :val true
                              :via [::spec/attrs]})
          ::s/spec ::spec/attrs
          ::s/value {:a true}}
         (s/explain-data ::spec/attrs {:a true}))))

(deftest test-long-attr-int
  (is (= {::s/problems (list {:in [:a 1]
                              :path [1]
                              :pred 'clojure.core/string?
                              :val 324
                              :via [::spec/attrs]})
          ::s/spec ::spec/attrs
          ::s/value {:a 324}}
         (s/explain-data ::spec/attrs {:a 324}))))

(deftest test-long-attr-dec
  (is (= {::s/problems (list {:in [:a 1]
                              :path [1]
                              :pred 'clojure.core/string?
                              :val 4.5
                              :via [::spec/attrs]})
          ::s/spec ::spec/attrs
          ::s/value {:a 4.5}}
         (s/explain-data ::spec/attrs {:a 4.5}))))

(deftest test-long-attr-fraction-1
  (is (= {::s/problems (list {:in [:a 1]
                              :path [1]
                              :pred 'clojure.core/string?
                              :val 7/22
                              :via [::spec/attrs]})
          ::s/spec ::spec/attrs
          ::s/value {:a 7/22}}
         (s/explain-data ::spec/attrs {:a 7/22}))))

(deftest test-long-attr-fraction-2
  (is (= {::s/problems (list {:in [:a 1]
                              :path [1]
                              :pred 'clojure.core/string?
                              :val 22/7
                              :via [::spec/attrs]})
          ::s/spec ::spec/attrs
          ::s/value {:a 22/7}}
         (s/explain-data ::spec/attrs {:a 22/7}))))

(deftest test-long-attr-date
  (let [some-date (t/date-time 2014 5 17 22 33 45)]
    (is (= {::s/problems (list {:in [:a 1]
                                :path [1]
                                :pred 'clojure.core/string?
                                :val some-date
                                :via [::spec/attrs]})
            ::s/spec ::spec/attrs
            ::s/value {:a some-date}}
           (s/explain-data ::spec/attrs {:a some-date})))))

(deftest test-long-attr-file
  (let [some-file (io/file "hi-there.txt")]
    (is (= {::s/problems (list {:in [:a 1]
                                :path [1]
                                :pred 'clojure.core/string?
                                :val some-file
                                :via [::spec/attrs]})
            ::s/spec ::spec/attrs
            ::s/value {:a some-file}}
           (s/explain-data ::spec/attrs {:a some-file})))))

(deftest test-long-attr-url
  (let [some-url (URL. "http://www.imdb.com")]
    (is (= {::s/problems (list {:in [:a 1]
                                :path [1]
                                :pred 'clojure.core/string?
                                :val some-url
                                :via [::spec/attrs]})
            ::s/spec ::spec/attrs
            ::s/value {:a some-url}}
           (s/explain-data ::spec/attrs {:a some-url})))))

(deftest test-long-attr-uri
  (let [some-uri (URI. "hi-there.txt")]
    (is (= {::s/problems (list {:in [:a 1]
                                :path [1]
                                :pred 'clojure.core/string?
                                :val some-uri
                                :via [::spec/attrs]})
            ::s/spec ::spec/attrs
            ::s/value {:a some-uri}}
           (s/explain-data ::spec/attrs {:a some-uri})))))

(deftest test-long-attr-path
  (let [some-path (.toPath ^File (io/file "hi-there.txt"))]
    (is (= {::s/problems (list {:in [:a 1]
                                :path [1]
                                :pred 'clojure.core/string?
                                :val some-path
                                :via [::spec/attrs]})
            ::s/spec ::spec/attrs
            ::s/value {:a some-path}}
           (s/explain-data ::spec/attrs {:a some-path})))))


(deftest test-long-children
  (is (= nil (s/explain-data ::spec/content [])))

  (is (= nil (s/explain-data ::spec/content ["this is a string node"])))

  (is (= nil (s/explain-data ::spec/content [{:tag :some-node}])))

  (is (= nil (s/explain-data ::spec/content ["this is a string node" "this is another string node"])))

  (is (= nil (s/explain-data ::spec/content [{:tag :some-node} {:tag :another-node}])))

  (is (= nil (s/explain-data ::spec/content ["this is a string node" "this is another string node" {:tag :some-node} {:tag :another-node}])))

  (is (= nil (s/explain-data ::spec/content ["this is a string node" {:tag :some-node} "this is another string node" {:tag :another-node}])))

  (is (= nil (s/explain-data ::spec/content [{:tag :some-node} {:tag :another-node} "this is a string node" "this is another string node"])))

  (is (= nil (s/explain-data ::spec/content [{:tag :some-node} "this is a string node" {:tag :another-node} "this is another string node"])))

  (is (= nil (s/explain-data ::spec/content [{:tag :some-node} "this is a string node" "this is another string node" {:tag :another-node}])))

  (is (= nil (s/explain-data ::spec/content ["this is a string node" {:tag :some-node} {:tag :another-node} "this is another string node"])))

  (is (= {::s/problems (list {:in []
                              :path []
                              :pred 'clojure.core/vector?
                              :val "this is a string node"
                              :via [::spec/content]})
          ::s/spec ::spec/content
          ::s/value "this is a string node"}
         (s/explain-data ::spec/content "this is a string node")))

  (is (= {::s/problems (list {:path [:node]
                              :pred 'map?
                              :val 1
                              :via [::spec/content ::spec/long-node]
                              :in [0]}
                             {:path [:string]
                              :pred 'clojure.core/string?
                              :val 1
                              :via [::spec/content]
                              :in [0]})
          ::s/spec ::spec/content
          ::s/value [1]}
         (s/explain-data ::spec/content [1])))
  )

(deftest test-short-node
  (is (= nil (s/explain-data ::spec/short-node [:a 1 2 3 "4" true]))))






