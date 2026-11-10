(ns samples.records)

; Interfaces, classes, and structs use C#-shaped type hints.
(definterface IArea "Computes the area of a shape."
  (^double Area [this]))

(defclass Circle "A circle with a radius."
  [^double radius]
  (implements IArea))

(defmethod IArea.Area "Returns the area of the circle."
  [^Circle this]
  (* System.Math/PI (.-radius this) (.-radius this)))

(defmethod Circle.Diameter "Returns the circle diameter."
  [^Circle this]
  (* 2.0 (.-radius this)))

(defmethod Circumference "Returns the circle circumference."
  [^Circle this]
  (* 2.0 System.Math/PI (.-radius this)))

(doc Circle)
(doc Circle.Diameter)

(def circle (Circle {:radius 2.0}))
(println "circle:" (.Area circle) (.Diameter circle) (.Circumference circle))

(defstruct Pair [^int a ^int b])
(def pair (Pair {:a 3 :b 4}))
(println "struct:" (.-a pair) (.-b pair))

; Constructors use the class name and may be overloaded.
(defclass PairMath [a b])
(defmethod PairMath [a b] {:a a :b b})
(defmethod PairMath.Sum [^PairMath this] (+ (.-a this) (.-b this)))

(def pair-math (PairMath 20 22))
(println "pair math:" (.-a pair-math) (.-b pair-math) (.Sum pair-math))

; Generic headers use C# declaration shape.
(defclass [Box T]
  [^T value])

(def box (new [Box Int] 42))
(println "generic:" (.-value box) (type (.-value box)))

(defclass [ComparableBox T (where [IComparable T])]
  [^T value])

(def comparable (new [ComparableBox Int] 7))
(println "constrained:" (.-value comparable))

(defclass DictBox [^[Dictionary [String Int]] data])
(def dict-box (->DictBox {"a" 1 "b" 2}))
(println "typed dict:" (.-Count (.-data dict-box)) (get (.-data dict-box) "a"))

; CLR calls the emitted interface stub when sorting.
(defclass SortBox [x]
  (implements System.IComparable))

(defmethod System.IComparable.CompareTo [^SortBox this other]
  (- (.-x this) (.-x other)))

(def sorted (System.Array/CreateInstance (type (SortBox {:x 0})) 3))
(.SetValue sorted (SortBox {:x 5}) 0)
(.SetValue sorted (SortBox {:x 2}) 1)
(.SetValue sorted (SortBox {:x 9}) 2)
(System.Array/Sort sorted)
(println "interface sort:"
  (.-x (.GetValue sorted 0))
  (.-x (.GetValue sorted 1))
  (.-x (.GetValue sorted 2)))
