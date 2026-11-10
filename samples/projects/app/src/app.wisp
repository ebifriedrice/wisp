(ns demo.app
  "The demo application."
  (:require [demo.math :as math]))

(defclass Later [^int n])

(defclass Holder [^Later value])

(declare helper)

(defn main []
  (println "sum:" (math/add 20 22))
  (println "forward:" (helper))
  (println "library-forward:" (math/later))
  (println "type:" (.-n (.-value (Holder {:value (Later {:n 7})})))))

(defn helper []
  "declared before use")
