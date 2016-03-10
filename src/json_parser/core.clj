(ns json-parser.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:import (java.io BufferedReader FileReader)))

(use '[clojure.string :only (trim trim-newline)])

(declare object-parser)
(declare array-parser)
(declare parser-factory)

(defn process-file [file-name]
  (slurp file-name))

(defn -main [& args]
  (println (str (parser-factory [array-parser object-parser] (process-file "/home/lein/file/test.json")))))

(defn boolean-parser [input-string]
   (cond
    (< (count input-string) 4) nil 
    (and (< (count (trim input-string)) 5) (not= (subs input-string 0 4) "true"))  nil
    (= (subs input-string 0 4) "true") [true (trim (subs input-string 4))]
    (= (subs input-string 0 5) "false") [false (trim (subs input-string 5))]
    (> (count input-string) 4) nil))

(defn null-parser [input-string]
  (cond
   (< (count input-string) 4) nil
   (= (subs input-string 0 4) "null") [nil (trim (subs input-string 4))]
    "default" nil)) 

(defn string-parser [input-string]
  (if (= (nth input-string 0) \")
    [(trim (clojure.string/replace (subs input-string 1 (+ (.indexOf (subs input-string (+ (.indexOf input-string "\"") 1)) "\"") 1)) #"\\.*" ""))
     (trim (subs input-string (+ (.indexOf (subs input-string (+ (.indexOf input-string "\"") 1)) "\"") 2)))]
    nil))

(defn comma-parser [input-string]
  (if (= (nth (trim input-string) 0) \,)
    [(trim (subs (trim input-string) 1))]
    nil))

(defn colon-parser [input-string]
  (if (= (nth (trim input-string) 0) \:)
    [(trim (subs (trim input-string) 1))]
    nil))

(defn number-parser [input-string]
  (loop [x (trim input-string), num []]
    (if (or (= (nth (trim x) 0) \:) (= (nth (trim x) 0) \,) (= (nth (trim x) 0) \]) (= (nth (trim x) 0) \}))
      (let [number (apply str num)]
        (if (or (not= (re-matches #"\d+" number) nil) (not= (re-find #"\d+.+e+\d+" number) nil) (not= (re-find #"\d+.+\d+" number) nil))
          [(read-string number) x]
          nil)) 
      (recur (trim (subs x 1)) (conj num (nth x 0))))))

(defn parser-factory [func input-string]
  (when (not (empty? func))
    (loop [x func]
      (when (not (empty? x))
        (if (not= ((first x) (trim input-string)) nil)
          ((first x) (trim input-string))
          (recur (drop 1 x)))))))

(defn object-parser [input-string]
  (if (= (nth (trim input-string) 0) \{)  
    (loop [x (trim (subs (trim (str input-string)) 1)), accumulating-total {}]
      (if (not= (string-parser x) nil)
        (let [remaining (string-parser x)]
          (if (not= (colon-parser (last remaining)) nil)
            (let [parsed (parser-factory [array-parser comma-parser object-parser boolean-parser string-parser null-parser number-parser] (trim (str (last (colon-parser (last remaining))))))]
              (if (= (nth (trim (last parsed)) 0) \})
                (if (= (count (last parsed)) 1) (conj accumulating-total [(first remaining) (first parsed)]) [(conj accumulating-total [(first remaining) (first parsed)]) (subs (trim (last parsed)) 1)])
                (recur (last (comma-parser (last parsed))) (conj accumulating-total [(first remaining) (first parsed)] )))) 
            nil))
        nil))
    nil))

(defn array-parser [input-string]
  (if (= (nth (trim input-string) 0) \[) 
    (loop [x (trim (subs (trim (str input-string)) 1)), accumulating-total []]
      (let [parsed (parser-factory [array-parser comma-parser object-parser number-parser boolean-parser string-parser null-parser] x)]
       (if (= (nth x 0) \])
         (if (= (count x) 1) accumulating-total [accumulating-total (trim (subs x 1))])
         (recur (trim (str (last parsed))) (if (= (count parsed) 1) accumulating-total (conj accumulating-total (first parsed)))))))
    nil))
