(ns trie-search.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; The root node is :head
;; An empty trie is {:head {}}
;; The word termination marker is {:nil nil})

(defn- file->line-seq [f]
  (let [dict (with-open [r (io/reader f)]
               (doall (line-seq r)))]
    dict))

(defn word->keyword-seq [word]
  (cons :head
        (map (comp keyword str)
             (seq (str/lower-case word)))))

(defn add-word [t word]
  (assoc-in t
            (word->keyword-seq word)
            termination-marker))

(defn add-words [t words]
  (reduce (comp add-word) t words))

(defn add-sentence [t sentence]
  (add-words t (str/split sentence #" ")))

(defn file->trie [file]
  (loop [words (file->line-seq file)
         t (make-empty-trie)]
    (if (seq words)
      (recur (rest words)
             (add-word t (first words)))
      t)))

(defn make-empty-trie []
  {:head {}})

(defn word->trie [word]
  (add-word (make-empty-trie) word))

(defn words->trie [words]
  (add-words (make-empty-trie) words))

(defn sentence->trie [sentence]
  (add-sentence (make-empty-trie) sentence))

(defn match [trie word]
  (if (> 3 (count word))
    []
    (letfn [(search [node w acc]
              (mapcat (fn [[k v]]
                        (if (= :nil k)
                          (cons w acc)
                          (search v (str w (name k)) acc)))
                      node))]
      (search (get-in trie (word->keyword-seq word))
              word
              []))))

