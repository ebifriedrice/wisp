(ns samples.control)

(println "sum:"
  (loop [i 0 acc 0]
    (if (= i 10)
      acc
      (recur (inc i) (+ acc i)))))

(defn factorial [n acc]
  (if (= n 0)
    acc
    (recur (dec n) (* acc n))))

(println "factorial:" (factorial 6 1))

(println "caught:"
  (try
    (throw "boom")
    (catch e (.-Message e))))

(println "typed:"
  (try
    (throw (new System.InvalidOperationException "typed boom"))
    (catch System.ArgumentException e :argument)
    (catch System.InvalidOperationException e (.-Message e))
    (catch System.Exception e :other)))

(println "fallback:"
  (try
    (throw (new System.ArgumentException "argument boom"))
    (catch System.InvalidOperationException e :wrong)
    (catch e :untyped)))

(try
  (println "body")
  (finally (println "cleanup")))
