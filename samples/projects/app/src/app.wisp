(ns demo.app
  "The demo application."
  (:require [demo.math :as math]))

(defclass Holder [^Later value])

(defclass Later [^int n])

(defn main []
  (println "sum:" (math/add 20 22))
  (println "forward:" (helper))
  (println "library-forward:" (math/later))
  (println "type-forward:" (.-n (.-value (Holder {:value (Later {:n 7})})))))

; Defined after main: the project loader's declaration pass handles this.
(defn helper []
  "no pre-declaration")
