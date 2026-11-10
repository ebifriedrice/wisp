(ns samples.interop)

(println "PI:" (System.Math/PI))
(println "abs:" (System.Math/Abs -42))
(println "concat:" (System.String/Concat "wisp" " + CLR"))

(def builder (System.Text.StringBuilder. "wisp"))
(.Append builder " + CLR")
(println "builder:" (.ToString builder))

(def numbers (new [:List :Int]))
(.Add numbers 10)
(.Add numbers 20)
(println "count:" (.-Count numbers))
