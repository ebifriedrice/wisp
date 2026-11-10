(ns samples.matching)

(defunion Shape
  (Circle [radius])
  (Rect [width height])
  (Empty))

(def circle (Circle {:radius 3}))
(def rect (Rect {:width 4 :height 5}))
(def nothing (Empty))

(defn area [shape]
  (match shape
    ((Circle radius) (* radius radius))
    ((Rect width height) (* width height))
    ((Empty) 0)))

(println "areas:" (area circle) (area rect) (area nothing))

(println "guards:"
  (match circle
    ((Circle radius) :when (> radius 0) (* 2 radius))
    ((Circle radius) radius)
    (_ 0)))

(println "collections:"
  (match [1 2 3]
    ([a b] (+ a b))
    ([a b c] (+ a b c))
    (_ -1))
  (match {:a 1 :b 2}
    ({:keys [a b]} (+ a b))
    (_ -1)))

(println "type pattern:"
  (match circle
    (^Circle c (.-radius c))
    (_ 0)))

(defactive even? [n]
  (when (= 0 (mod n 2)) n))

(println "active:"
  (match 4
    ((active even? x) (* x 2))
    (_ -1))
  (match 3
    ((active even? x) (* x 2))
    (_ -1)))
