(ns samples.hello)

(defn greet [name]
  (str "Hello, " name "!"))

(println (greet "Wisp"))

(defn fact [n]
  (if (= n 0)
    1
    (* n (fact (- n 1)))))

(println "6! =" (fact 6))
