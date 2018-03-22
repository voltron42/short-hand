(ns short-hand.pred)

(defn matches? [regex]
  (fn [value]
    (let [matches (re-matches regex value)
          matches (if (coll? matches) matches [matches])]
      (contains? (set matches) value))))

(defn named-as
  ([my-namespace my-name]
   (fn [value]
     (try
       (and ((matches? my-namespace) (namespace value)) ((matches? my-name) (name value)))
       (catch Throwable t
         false))))
  ([my-name]
   (fn [value]
     (try
       (and (nil? (namespace value)) ((matches? my-name) (name value)))
       (catch Throwable t
         false)))))