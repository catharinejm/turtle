(ns turtle.haml
  (:use hiccup.core
        [hiccup.page :only (doctype)])
  (:require [clojure.string :as str]))

(defn exception [^String msg]
  (proxy [RuntimeException] [^String msg]))

(def doctypes #{:html4 :html5})

(defn parse-doctype [dt-line]
  (let [[_ dt-str] (re-find #"^!!!(.*)$" dt-line)
        dt (doctypes (-> dt-str
                         (or "html5")
                         str/trim
                         keyword))]
    (or dt
        (throw (exception (str "Invalid doctype - \"" (str/trim dt-str) "\""))))))

(def nest-width 2)

(def tag-end-delims #{\{ \( \space})
(defn tag-chr? [c] (not (tag-end-delims c)))

(defn get-tag [line]
  (let [[tag rem] (split-with tag-chr? line)]
    (cond
      (= (first tag) \%)
      [(apply str (rest tag)) rem]
      (#{\# \.} (first tag))
      [(apply str "div" tag) rem]
      :otherwise [nil line])))

(defn get-attrs [line]
  (if (= (first line) \{)
    (let [[attr-chrs rem] (split-with #(not= \} %) line)]
      (try
        (let [attr-map (read-string (str (apply str attr-chrs) (first rem)))]
          [attr-map (rest rem)])
        (catch RuntimeException e
          (throw (exception (str "Invalid attribute map - \"" line "\""))))))
    [nil line]))

(defn build-element [line]
  (let [line (str/trim line)
        [tag line] (get-tag line)
        [attrs line] (get-attrs line)]
    (if tag
      (vector tag attrs (apply str line))
      (apply str line))))

(defn get-level [line]
  (if (seq line)
    (/ (count (take-while #{\space} line)) nest-width)
    0))

(defn vlast [vec]
  (first (rseq vec)))

(defn valid-nest?
  "from and to are levels!"
  [from to]
  (and (integer? from)
       (integer? to)
       (or (= from to)
           (= (inc from) to)
           (< to from))))

(defn parse-level [lines level]
  (loop [tags []
         [cur :as lines] lines]
    (println "Line: " cur)
    (println "Level: " level)
    (println "Tags: " tags)
    (if cur
      (let [new-level (get-level cur)]
        (if-not (valid-nest? level new-level)
          (throw (exception (str "Invalid nesting - \"" (first rem) "\"")))
          (cond
            (= new-level level)
            (recur (conj tags (build-element cur))
                   (next lines))
            (> new-level level)
            (let [[next-tags rem] (parse-level lines new-level)]
              (recur (update-in tags [(dec (count tags))] into next-tags)
                     rem))
            (< new-level level)
            [tags lines])))
      [tags nil])))

(defn parse-lines
  [lines]
  (apply #(html %&) (first (parse-level (filter (complement str/blank?) lines) 0))))

(defn read-file [file]
  (let [lines (str/split-lines (slurp file))]
    (cond
      (re-find #"^\s+" (first lines))
      (throw (exception "Invalid nesting - First line is indented."))
      (re-find #"^!!!" (first lines))
      (parse-lines (parse-doctype (first lines)) (rest lines))
      :otherwise (parse-lines lines))))