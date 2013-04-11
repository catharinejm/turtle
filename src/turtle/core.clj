(ns turtle.core
  (:use net.cgrand.enlive-html)
  (:require [clojure.string :as s]))

(def val "Value!")
(defmacro silly-macro [n]
  `(* ~n 2))
(def users [{:name "Bill" :hobbies ["food" "turtles" "bats"]}
            {:name "Timmy" :hobbies ["nosepicking"]}
            {:name "Buttpants"}])

(defn as-coll [form]
  (if (coll? form)
    form
    (list form)))

(defn capture [& forms]
  (interleave forms (repeat "\n")))

(defmacro print-each [bind & forms]
  `(flatten (for [~@bind] (capture ~@forms))))

(defn render*
  [html pfn]
  (pfn (s/join (emit* html))))

(defn render
  ([html]
     (render* html println))
  ([html file]
     (render* html (partial spit file))))

(def ^:private turtle-re #"\(\{(.*?)\}\)")

(defn read-content [content]
  (loop [in-strs []
         [cur & rem] content
         tags {}]
    (if cur
      (if (map? cur)
        (let [tag-sym (gensym "__turtle__tag__")]
          (recur (conj in-strs `(~tag-sym ~(read-content (:content cur))))
                 rem
                 (assoc tags tag-sym (assoc cur :content nil))))
        (recur (conj in-strs cur)
               rem
               tags))
      (let [body-forms (read-string (s/join " " in-strs))]
        (if (empty? tags)
          body-forms
          (letfn [(tag-fn [[fn-name tag]]
                    `(~fn-name [~'c] (assoc ~tag :content (as-coll ~'c))))]
            `(letfn [~@(map tag-fn tags)]
               ~body-forms)))))))

(defn eval-and-replace [{content :content :as tag}]
  (let [body-form (read-content content)]
    (as-coll (eval body-form))))

(defn munge-html [file]
  (let [html (html-resource file)
        trans (transformation [:clj] eval-and-replace)]
    (trans html)))
