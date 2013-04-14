(ns turtle.haml
  (:use hiccup.core
        [hiccup.page :only (doctype) :rename {doctype doctype-map}])
  (:require [clojure.string :as str]))

(defn exception
  ([^String msg]
     (proxy [RuntimeException] [^String msg]))
  ([msg obj & objs]
     (let [obj-strs (apply str (interpose "\" - \"" (cons obj objs)))
           full-msg (str msg " - \"" obj-strs "\"")]
       (exception full-msg))))

(defn parse-doctype [dt-line]
  (let [[_ dt-str] (re-find #"^!!!(.*)$" (or dt-line ""))
        dt-key (if (str/blank? dt-str)
                 :html5
                 (keyword (str/trim dt-str)))]
    (or (doctype-map dt-key)
        (throw (exception "Invalid doctype" (str/trim dt-str))))))

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
          (throw (exception "Invalid attribute map" (apply str line))))))
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

(defn spacify-content [tag]
  (if (string? tag)
    (str " " tag)
    tag))

(defn parse-level [lines level]
  (loop [tags []
         [cur :as lines] lines]
    (if cur
      (let [new-level (get-level cur)]
        (if-not (valid-nest? level new-level)
          (throw (exception "Invalid nesting" (apply str cur)))
          (cond
            (= new-level level)
            (recur (conj tags (build-element cur))
                   (next lines))
            (> new-level level)
            (let [[next-tags rem] (parse-level lines new-level)]
              (recur (update-in tags [(dec (count tags))] into (map spacify-content next-tags))
                     rem))
            (< new-level level)
            [tags lines])))
      [tags nil])))

(defn parse-lines
  [lines]
  (let [lines (filter (complement str/blank?) lines)
        doctype-str (re-find #"^!!!" (first lines))
        doctype (parse-doctype doctype-str)
        lines (if doctype-str (rest lines) lines)]
    (if-not (zero? (get-level (first lines)))
      (throw (exception "Invalid nesting - First line is indented."))
      (let [[tags] (parse-level lines 0)]
        (html doctype (seq tags))))))

(defn read-file [file]
    (let [lines (str/split-lines (slurp file))]
      (parse-lines lines)))