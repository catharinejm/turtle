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

(defn get-tag [line]
  (vector line))

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
  (loop [[cur :as lines] lines
         tags []]
    (if cur
      (let [new-level (get-level cur)]
        (if-not (valid-nest? level new-level)
          (throw (exception (str "Invalid nesting - \"" (first rem) "\"")))
          (cond
            (= new-level level)
            (recur (next lines)
                   (conj tags (get-tag cur)))
            (> new-level level)
            (let [[rem next-tags] (parse-level lines new-level)]
              (recur rem
                     (conj tags (into (vlast tags) next-tags))))
            (< new-level level)
            [lines tags])))
      [nil tags])))

(defn parse-lines
  ([lines]
     (parse-lines :html5 lines))
  ([dtd lines]
     #_(html (doctype dtd)
             (parse-level lines 0))
     (parse-level lines 0)))

(defn read-file [file]
  (let [lines (str/split-lines (slurp file))]
    (cond
      (re-find #"^\s+" (first lines))
      (throw (exception "Invalid nesting - First line is indented."))
      (re-find #"^!!!" (first lines))
      (parse-lines (parse-doctype (first lines)) (rest lines))
      :otherwise (parse-lines lines))))