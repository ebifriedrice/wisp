(ns samples.delegates)

(defdelegate ^int IntUnary "An integer transform." [^int x])
(doc IntUnary)

(def twice "Twice a number, as a delegate." (delegate IntUnary (fn [x] (* x 2))))
(println "twice:" (twice 21))

(def inc (delegate IntUnary (fn [x] (+ x 1))))
(set! twice inc)
(println "reassigned:" (twice 41))

(defclass Holder [^IntUnary callback])
(def holder (Holder {:callback (fn [x] (* x 3))}))
(println "field delegate:" ((.-callback holder) 4))

(defdelegate PrintOne [^String message])
(def printer (delegate PrintOne (fn [message] (println "printer:" message))))
(printer "hello")

(defstruct Point "A 2D point." [^int x])

(defdelegate ^Point MovePoint "Moves a point by one." [^Point p])
(def move-point (delegate MovePoint (fn [p] (Point {:x (+ (.-x p) 1)}))))
(println "wisp type delegate:" (.-x (move-point (Point {:x 4}))))

(defdelegate ^int ReadInt [^Int32* p])
(def read-int (delegate ReadInt (fn [p] (System.Runtime.InteropServices.Marshal/ReadInt32 p))))
(def memory (System.Runtime.InteropServices.Marshal/AllocHGlobal 4))
(System.Runtime.InteropServices.Marshal/WriteInt32 memory 42)
(println "pointer delegate:" (read-int memory))
(System.Runtime.InteropServices.Marshal/FreeHGlobal memory)

(set! twice nil)
(println "cleared:" twice)
