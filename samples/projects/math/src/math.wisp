(ns demo.math "Small math helpers.")

(defn add [a b] (+ a b))

(declare defined-later)

(defn later [] (defined-later))

(defn defined-later [] 99)
