(ns demo.lib "The **library** namespace.")

(def answer 42)

(ns demo.app
  (:import [System.Text StringBuilder])
  (:require [demo.lib :as lib :refer [answer]]))

(def builder (StringBuilder. "wisp"))
(.Append builder " namespaces")
(println "import:" (.ToString builder))
(println "require:" lib/answer "refer:" answer)
(println "auto-keyword:" ::thing)

(ns demo.uses (:use demo.lib))
(println "use:" answer)

(ns demo.only (:use [demo.lib :only [answer]]))
(println "only:" answer)

(in-ns 'demo.other)
(def x 7)
(println "in-ns:" x ::local)
