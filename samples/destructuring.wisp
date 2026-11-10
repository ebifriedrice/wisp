(ns samples.destructuring)

(let [[a b & rest] [1 2 3 4]]
  (println "vector:" a b rest))

(let [{:keys [a b] :or {b 9} :as m} {:a 1}]
  (println "map:" a b m))

(defn add-pair [[a b]]
  (+ a b))

(println "fn:" (add-pair [20 22]))

(println "loop:"
  (loop [[i acc] [0 0]]
    (if (= i 4)
      acc
      (recur [(inc i) (+ acc i)]))))
