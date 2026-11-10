(ns samples.async)

(defn work [n]
  (async
    (println "start" n)
    (await (System.Threading.Tasks.Task/Delay 10))
    (println "after" n)
    (+ n 1)))

(def result (await (work 41)))
(println "result:" result)

(def nested
  (async
    (let [a (await (async 10))
          b (await (async 20))]
      (+ a b))))

(println "nested:" (await nested))

(def caught
  (async
    (try
      (await (async (throw "boom")))
      (catch e "caught"))))

(println "caught:" (await caught))

(def value-task (^[ValueTask Int] async (+ 40 2)))
(println "value task:" (await value-task))

(def unit-task (^ValueTask async (await (System.Threading.Tasks.Task/Delay 1))))
(await unit-task)
(println "unit value task complete")
