(ns samples.macros)

(defmacro unless [test then else]
  (list 'if test else then))

(println (unless false "then branch" "else branch"))

(defmacro all [& forms]
  (cons 'do forms))

(println "all:" (all 1 2 3))
