(ns turtle.core
  (:use net.cgrand.enlive-html
        [evalive.core :only (evil)])
  (:require [clojure.string :as s]
            [fogus.lexical.chocolate :as lex]))

(def val "Value!")
(defmacro silly-macro [n]
  `(* ~n 2))
(def users [{:name "Bill"}
            {:name "Timmy"}
            {:name "Buttpants"}])

; Thanks to Fogus & Alan Dipert
(defmacro lexical-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym))
                 symbols)
            symbols)))

(defn qsym [s]
  `(quote ~s))

(defmacro print-each [bind body]
  `(doall (interpose "\n" (for [~@bind] ~body))))

(defn render*
  [html pfn]
  (pfn (apply str (emit* html))))

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
                    `(~fn-name [~'c] (assoc ~tag :content ~'c)))]
            `(letfn [~@(map tag-fn tags)]
               ~body-forms)))))))

(defn eval-and-replace [{content :content :as tag}]
  (let [body-form (read-content content)
        final-form (eval body-form)]
    (if (coll? final-form)
      final-form
      (list final-form))))

(defn munge-html [file]
  (let [html (html-resource file)
        trans (transformation [:clj] eval-and-replace)]
    (trans html)))
