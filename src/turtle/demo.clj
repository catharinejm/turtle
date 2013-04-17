(ns turtle.demo
  (:use [turtle.core :only (render munge-html print-each capture)]
        [turtle.haml :only (read-file)])
  (:require [clojure.string :as s]))

(def value "Value!")
(defmacro silly-macro [n]
  `(* ~n 2))
(def users [{:name "Bill" :hobbies ["food" "turtles" "bats"]}
            {:name "Timmy" :hobbies ["nosepicking"]}
            {:name "Buttpants"}])

(defn demo [file pfn]
  (println "Input:\n\n")
  (println (slurp (str "templates/" file)))
  (println "\n\nOutput:\n\n")
  (pfn))

(defn html-demo []
  (demo "test.html"
        #(render (munge-html "test.html"))))

(defn haml-demo []
  (demo "test.haml"
        #(println (read-file "templates/test.haml"))))