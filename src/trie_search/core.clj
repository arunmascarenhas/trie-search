(ns trie-search.core
  (:require [clojure.string :as str]))

(def trie {:head {}})

(def termination-marker {:nil nil})

(defn word->keyword-seq [word]
  (cons :head
        (map (comp keyword str)
             (seq word))))

(defn add-word [t word]
  (assoc-in t
            (word->keyword-seq word)
            termination-marker))

(defn add-words [t words]
  (reduce (comp add-word) t words))

(defn add-sentence [t sentence]
  (add-words t (str/split sentence #" ")))

(defn- walk-helper [sub-t curr-word acc]
  (cond 
   (= sub-t termination-marker) (cons curr-word acc)
   (contains? sub-t :nil) (recur (dissoc sub-t :nil)
                                               curr-word
                                               (cons curr-word acc))
;;   :else (loop [t-keys (keys sub-t)
;;                sub-t sub-t
;;                curr-word curr-word
;;                acc acc]
;;(;; logic here))))
   :else acc))

(defn walk [t word]
  (let [sub-t (get-in t (word->keyword-seq word))]
    (walk-helper sub-t word [])))


