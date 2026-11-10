(ns samples.reader)

; Reader conditionals select on :wisp / :clr, with :default fallback.
(println "selected:" #?(:wisp "wisp" :clr "clr" :default "other"))
(println "spliced:" [0 #?@(:wisp [1 2] :default [9]) 3])
(println "prefix splice:" [0 '#?@(:wisp [1 2]) 3])

; Syntax quote expands at read time, including unquote, splicing, and auto-gensym.
(defmacro unless [test then else]
  `(if ~test ~else ~then))

(println (unless false "then" "else"))

(defmacro twice [x]
  `(let [v# ~x] (+ v# v#)))

(println "twice:" (twice 21))

(defmacro all [& forms]
  `(do ~@forms))

(println "all:" (all 1 2 3))

; Nested syntax quote: the inner expansion is data for the outer quote.
(defmacro make-printer [message]
  `(defmacro print-it [] `(println ~~message)))

(make-printer "nested hello")
(print-it)

; Map templates splice at the flat key/value argument level.
(println "map splice:" `{1 ~@[2 3] 4 ~@[5 6]})
