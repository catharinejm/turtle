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

(defmacro capture [binds body]
  `(doall (for [~@binds]
            ~body)))

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
         [cur :as rem] content
         tags {}]
    (if rem
      (if (map? cur)
        (let [sym (gensym "turtle__eval")]
          (recur (conj in-strs (name sym))
                 (next rem)
                 (assoc tags sym (update-in cur [:content] read-content))))
        (recur (conj in-strs cur)
               (next rem)
               tags))
      (let [body-forms (read-string (s/join " " in-strs))]
        (if (empty? tags)
          body-forms
          `(let [{:syms [~@(keys tags)]} '~tags]
            ~body-forms))))))

(defn eval-and-replace [{content :content :as tag}]
  (let [body-form (read-content content)]
    (eval body-form)))

(comment "Old definition"
  (defn eval-and-replace [{content-seq :content {for-binding :for if-binding :if} :attrs :as tag}]
    (when (seq content-seq)
      (cond
        if-binding
        (when (eval (read-string if-binding))
          (eval-and-replace (update-in tag [:attrs] dissoc :if)))
        for-binding
        (let [bind-form (read-string for-binding)
              bind-contexts (eval `(for ~(read-string for-binding) (lex/context)))
              needed-syms (set (filter symbol? (flatten bind-form)))]
          (letfn [(gen-tags [scope cseq]
                    (mapv (fn [node]
                            (if (map? node)
                              (update-in node [:content] (partial gen-tags scope))
                              (eval-body scope node)))
                          cseq))
                  (eval-body [scope body-str]
                    (s/replace body-str turtle-re
                               (fn [[_ body]]
                                 (let [forms (read-string body)
                                       true-scope (select-keys scope
                                                               (apply (partial conj needed-syms)
                                                                      (filter symbol? (flatten forms))))]
                                   (evil true-scope forms)))))]
            (flatten (mapv #(gen-tags % content-seq) bind-contexts))))
        :else
        (let [form (read-string (apply str content-seq))]
          (str (eval form)))))))

(defn munge-html [file]
  (let [html (html-resource file)
        trans (transformation [:clj] eval-and-replace)]
    (trans html)))
