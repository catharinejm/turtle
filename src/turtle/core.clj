(ns turtle.core
  (:use net.cgrand.enlive-html
        [clojure.string :only (join)])
  (:require [fogus.lexical.chocolate :as lex]))

(def val "Value!")
(defmacro silly-macro [n]
  `(* ~n 2))
(def users [{:name "Bill"}
            {:name "Timmy"}
            {:name "Buttpants"}])

(defn render*
  [html pfn]
  (pfn (apply str (emit* html))))

(defn render
  ([html]
     (render* html println))
  ([html file]
     (render* html (partial spit file))))

(defn- gen-tags [content-seq]
  )

(defn eval-and-replace [{content-seq :content {for-binding :for if-binding :if} :attrs :as tag}]
  (when (seq content-seq)
    (when (and if-binding (eval (read-string if-binding)))
      (eval-and-replace (update-in tag [:attrs] dissoc :if)))
    (if for-binding
      (let [body content-seq
            fb-form (read-string for-binding)
            emissions (doall (eval `(for ~fb-form
                                      (letfn [(gen-tags# [content-seq#]
                                                (mapv (fn [node#]
                                                        (if (map? node#)
                                                          (update-in node# [:content] gen-tags#)
                                                          (eval-body# node#)))
                                                      content-seq#))
                                              (eval-body# [body-str#]
                                                body-str#)]
                                        (gen-tags# '~body)))))]
        (flatten emissions))
      (let [form (read-string (apply str content-seq))]
        (str (eval form))))))

(defn munge-html [file]
  (let [html (html-resource file)
        trans (transformation [:clj] eval-and-replace)]
    (trans html)))
