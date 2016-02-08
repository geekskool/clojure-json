(ns json-parser.core
  (:gen-class)
  (:require [clojure.string :as str]))

(use '[clojure.string :only (split trim trim-newline)])

(defn -main
  "I don't do a whole lot ... yet."
  [& args])

(defn boolean-parser [input-string]
  "Boolean parser"
   (cond
    (< (count input-string) 4)  nil
    (= (subs input-string 0 4) "true") [true (subs input-string 4)]
    (= (subs input-string 0 5) "false") [false (subs input-string 5)]
    (> (count input-string) 4) nil))

(defn null-parser [input-string]
  "Null parser"
  (cond
   (< (count input-string) 4) nil
   (= (subs input-string 0 4) "null") [nil (subs input-string 4)]
    "default" nil)) 

(defn string-parser [input-string]
  (if (= (nth input-string 0) \")
    [(subs input-string 1 (+ (.indexOf (subs input-string (+ (.indexOf input-string "\"") 1)) "\"") 1))
     (subs input-string (+ (.indexOf (subs input-string (+ (.indexOf input-string "\"") 1)) "\"") 2))]
    nil))

(defn comma-parser [input-string]
  "Comma parser"
  (if (= (nth input-string 0) \,)
    (subs input-string 1)
    nil))

(defn parser-factory [func input-string]
  "Parser factory"
  (if (empty? func)
    nil
    (if (not= ((first func) input-string) nil)
      ((first func) input-string)
      (recur (drop 1 func) input-string))))

(defn array-parser [input-string]
  (if (= (nth input-string 0) \[) 
    ((def in-put (atom ""))
     (swap! in-put (fn [x] (subs input-string 1)))
     (str @in-put)
     (while ()
       (if (= (nth (trim @in-put) 0) \])
        (println "hello")
        (println "hi"))
       )
) nil ))


(defn input-reader []
  (def input (slurp "/home/pankaj/Documents/test.json"))
  (def parsed [])
  (parser-factory [array-parser] input)
)
