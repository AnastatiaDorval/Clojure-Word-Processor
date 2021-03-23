(defn num-with-word [word sentences]
   (count (filter #(contains? % word) (map #(apply hash-set %) sentences)))
)
 
;; now write a 2nd function that computes the percentage of sentences 
;; containing a given word
(defn pct-with-word [word sentences]
   (float (/ (num-with-word word sentences) (count sentences)))
)
 
;; test out on just sentences by Poe
(println "percent of words in Poe that are 'the'" (pct-with-word "the" eaps))
 
;; Now test out on the sentences of all of the authors
(println "percent of words in each of the authors sentences that are 'the'" (map #(pct-with-word "the" %) (map2 towords authors)))

(defn punctuation-pct [sentences]
      (float (/ (count (filter ispunc (flatten sentences))) (count (flatten sentences))))
)

;;print percentage of punctuation for each author
(println "percent of words in each of the authors' sentences that are punctuation" (map #(punctuation-pct %) sauthors))

(defn stopwords-pct [sentences]
      (float (/  (count (filter isstop (flatten sentences))) (count (flatten sentences))))
)

;;print percentage of stop words for each author
(println "percent of words in each of the authors' sentences that are stop words" (map #(stopwords-pct %) sauthors))

(defn coolwords-pct [sentences]
      (float (/ (count (remove ispunc (remove isstop (flatten sentences)))) (count (flatten sentences))))
)

;;print percentage of words that are not stop words or punctuation
(println "percent of words in each of the authors' sentences that are not stop words or punctuation" (map #(coolwords-pct %) sauthors))

;this finds the 10 most commonly used words in the sentence that are not stop words or punctuation
(defn common-words [sentences]
      (take 10 (sort-by second > (frequencies (remove ispunc (remove isstop (flatten sentences))))))
)

;this finds the nth most commonly used words in the sentence that are not the stop words or punctuation
(defn nth-common [n sentences]
      (take n (sort-by second > (frequencies (remove ispunc (remove isstop (flatten sentences))))))
)

;this gives the 10 most common pairs of words
(defn most-common-pairs [sentences]
      (take 10 (sort-by second > (frequencies (map #(str/join %) (mapcat #(partition 2 1 %) sentences)))))
)

;this gives the y most common xgrams of words
(defn y-most-common-xgrams [y x sentences]
      (take y (sort-by second > (frequencies (map #(str/join %) (mapcat #(partition x 1 %) sentences)))))
)

;this finds the 5 most common colors used in the writing
(defn five-most-common-colors [sentences]
      (take 5 (sort-by second > (frequencies (filter iscolor (flatten sentences)))))
)

(defn most-common-color [sentences]
      (take 1 (sort-by second > (frequencies (filter iscolor (flatten sentences)))))
)
