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

(def tag-end-delims #{\{ \( \space \=})
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

(defn split-out-forms [line]
  (letfn [(split [s]
            (split-with (complement #{\= \(}) s))
          (escape [s form]
            (if (and (not (str/blank? (apply str form)))
                     (= (last s) \\))
              (let [[s* f] (split (rest form))]
                (escape (concat (butlast s)
                                (cons (first form) s*))
                        f))
              (if (= (first form) \=)
                [s (rest form)]
                [s form])))]
    (apply escape (split line))))

(defn parse-form [form-chr]
  (letfn [(new-reader [chrs]
            (java.io.PushbackReader.
             (java.io.CharArrayReader. (char-array chrs))))
          (bleed-reader [rdr len]
            (let [chary (char-array len)]
              (.read rdr chary)
              (apply str (take-while #(not= (int %) 0) chary))))
          (read-nested [rdr]
            (loop [forms []]
              (let [form (try
                           (read rdr)
                           (catch RuntimeException e))]
                (if form
                  (recur (conj forms form))
                  (list* forms)))))]
    (let [reader (new-reader form-chr)]
      (try
        (let [form (read reader)
              rest (bleed-reader reader (count form-chr))]
          [form rest])
        (catch RuntimeException e
          (let [fc (rest form-chr)
                reader (new-reader fc)
                form (read-nested reader)
                rest (bleed-reader reader (count fc))]
            [form rest]))))))

(defn parse-line [line]
  (if (str/blank? (apply str line))
    (list)
    (let [[str-part form-part] (split-out-forms line)
          string (apply str str-part)
          [form rem] (parse-form form-part)
          line-parts (filter #(if (string? %)
                                (not (str/blank? %))
                                (not (nil? %)))
                             [string form])]
      (concat line-parts (parse-line rem)))))

(defn build-element [line]
  (let [line (str/trim line)
        [tag line] (get-tag line)
        [attrs line] (get-attrs line)
        line (apply str line)]
    (if tag
      (list (into [tag attrs] (parse-line line)))
      (parse-line line))))

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

(defn append-elements [parent children]
  (if (vector? parent)
    (into parent children)
    (concat parent children)))

(defn parse-level [lines level]
  (loop [tags []
         [cur :as lines] lines]
    (if cur
      (let [new-level (get-level cur)]
        (if-not (valid-nest? level new-level)
          (throw (exception "Invalid nesting" (apply str cur)))
          (cond
            (= new-level level)
            (recur (into tags (build-element cur))
                   (next lines))
            (> new-level level)
            (let [[next-tags rem] (parse-level lines new-level)]
              (recur (update-in tags [(dec (count tags))] append-elements (map spacify-content next-tags))
                     rem))
            :otherwise [tags lines])))
      [tags nil])))

(defn build-tags
  [lines]
  (let [lines (filter (complement str/blank?) lines)
        doctype-str (re-find #"^!!!" (first lines))
        doctype (parse-doctype doctype-str)
        lines (if doctype-str (rest lines) lines)]
    (if-not (zero? (get-level (first lines)))
      (throw (exception "Invalid nesting - First line is indented."))
      (let [[tags] (parse-level lines 0)]
        [doctype tags]))))

(defn read-file [file]
  (let [lines (str/split-lines (slurp file))
        [doctype tags] (build-tags lines)]
    (html doctype (eval `(seq ~tags)))))
