(ns seccon.core
  (:use [clojure.string :only [split-line split]]))

(def alphabets 
  (let [z (concat (->> (range (int \A) (inc (int \Z))))
                  (->> (range (int \a) (inc (int \a)))))]
    (zipmap z (map char z))))

(defn x->alphabet [x]
  (or (16->alphabet x)
      (2->alphabet x)
      (8->alphabet x)
      (10->alphabet x)))

(defn 2->alphabet [x]
  (as-> x $
        (Integer/parseInt $ 2) 
        (get alphabets $)))

(defn 8->alphabet [x]
  (as-> x $
        (Integer/parseInt $ 8)
        (get alphabet $)))

(defn 16->alphabet [x]
  (as-> x $
        (Integer/parseInt $ 16)
        (get alphabet $)))

(defn solve [s]
  (as-> s $
        (split $ #" ")
        (x->alphabet $)
        (reduce str $)))
