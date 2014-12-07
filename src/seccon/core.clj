(ns seccon.core
  (:import (java.net Socket)
           (java.io PrintWriter InputStreamReader BufferedReader))
  (:use [clojure.string :only [split split-lines]]
        [clojure.java.io :only [resource input-stream output-stream]]
        [clojure.core.async :only [go go-loop chan <! >!]]))

(def pro2 "87 101 108 1100011 0157 6d 0145 040 116 0157 100000 0164 104 1100101 32 0123 69 67 0103 1001111 1001110 040 062 060 49 064 100000 0157 110 6c 0151 1101110 101 040 0103 1010100 70 101110 0124 1101000 101 100000 1010011 1000101 67 0103 4f 4e 100000 105 1110011 040 116 1101000 0145 040 1100010 0151 103 103 0145 1110011 0164 100000 1101000 0141 99 6b 1100101 0162 32 0143 111 1101110 1110100 101 0163 0164 040 0151 0156 040 74 0141 1110000 1100001 0156 056 4f 0157 0160 115 44 040 0171 1101111 117 100000 1110111 0141 0156 1110100 32 0164 6f 32 6b 1101110 1101111 1110111 100000 0164 1101000 0145 040 0146 6c 97 1100111 2c 100000 0144 111 110 100111 116 100000 1111001 6f 117 63 0110 1100101 0162 0145 100000 1111001 111 117 100000 97 114 0145 46 1010011 0105 0103 67 79 1001110 123 87 110011 110001 67 110000 1001101 32 55 060 100000 110111 0110 110011 32 53 51 0103 0103 060 0116 040 5a 0117 73 0101 7d 1001000 0141 1110110 1100101 100000 102 0165 0156 33")

(def alphabets 
  (let [z (range 32 (inc (int \~)))]
    (zipmap z (map char z))))

(defn two->alphabet [x]
  (as-> x $
        (Integer/parseInt $ 2) 
        (get alphabets $)))

(defn eight->alphabet [x]
  (as-> x $
        (drop 1 $)
        (apply str $)
        (Integer/parseInt $ 8)
        (get alphabets $)))

(defn sixteen->alphabet [x]
  (as-> x $
        (str "0x" $)
        (Integer/decode $)
        (get alphabets $)))

(defn ten->alphabet [x]
  (as-> x $
        (Integer/parseInt $)
        (get alphabets $)))

(defn x->alphabet [x]
  (let [len (count x)
        head (-> x first)]
    (cond (>= len 6) (two->alphabet x)
          (= head \0) (eight->alphabet x)
          (some (->> (range (int \a) (inc (int \z))) (map char) set) x) (sixteen->alphabet x)
          :else (ten->alphabet x))))

(defn solve-pro2 [s]
  (as-> s $
        (split $ #" ")
        (map x->alphabet $)
        (apply str $)))

;; choose the number

;;(def url "number.quals.seccon.jp")
;;
;;(def port 31337)
;;
;;(def out (chan))
;;
;;(def in (chan))
;;
;;(defn split-numbers [s]
;;  (as-> s $ 
;;        (split $ #", ") 
;;        (map #(Integer/parseInt %) $)))
;;
;;(defn write [msg]
;;  (go (println "Writing...")
;;      (doto (:out @conn)
;;        (.println (str msg "\r"))
;;        (.flush))
;;      msg)
;;
;;(defn conn-handler [conn]
;;  (go (while true (>! in (-> @conn :in .readLine))))
;;  (go-loop [last-msg ""
;;            msg (<! in)
;;            res nil]
;;         (when (nil? (:exit @conn))
;;           (println last-msg)
;;           (println msg)
;;           (recur msg (.readLine (:in @conn))
;;             (cond
;;               (re-find #"Timeout, bye." msg) (dosync (alter conn merge {:exit true}))
;;               (re-find #"Wrong, bye." msg) (dosync (alter conn merge {:exit true}))
;;               (= last-msg "") nil
;;               (re-find #"minimum" msg) (println "Write: " (write conn (->> last-msg split-numbers (apply min) str)))
;;               (re-find #"maximum" msg) (println "Write: " (write conn (->> last-msg split-numbers (apply max) str)))
;;               :else nil)))))
;;
;;(defn connect []
;;  (let [socket (Socket. url port)
;;        in (-> socket .getInputStream InputStreamReader. BufferedReader.)
;;        out (-> socket .getOutputStream PrintWriter.)
;;        conn (ref {:in in, :out out})]
;;    (conn-handler conn)
;;    conn))
;;
;;(defn solve-choose-the-number []
;;  (connect))

(defn filter-hex [s]
  (if (= (count s) 1)
    (str \0 s)
    s))

(defn get-reverse []
  (-> "Reverseit" resource input-stream))

(defn byte-seq
  [input-stream]
  (let [coll (repeatedly #(.read input-stream))
        len (.indexOf coll -1)]
    (take len coll)))

(defn solve-reverseit []
  (with-open [out (-> "solved.jpg" resource output-stream)]  
    (as-> (get-reverse) $
          (byte-seq $)
          (map #(Integer/toHexString %) $)
          (map filter-hex $)
          (flatten $)
          (apply str $)
          (reverse $)
          (partition 2 $)
          (map #(apply str %) $)
          (map #(Integer/parseInt % 16) $)
          (dorun (map #(.write out %) $)))))
