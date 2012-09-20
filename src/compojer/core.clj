(ns compojer.core (:gen-class))
    
(def notes [])

(def available-pitches (range 8))
(def available-durations (range 6))

(defn create-flat-distribution [length] (repeat length (/ 1 length)))

(def pitch-probs (create-flat-distribution (count available-pitches)))

(defn prob-blocks [distr-length index] 
    (assert (not (or (neg? index) (>= index distr-length)))) 
    (let [index-after-half? (> (inc index) (/ distr-length 2)) 
        index (if index-after-half? index (- distr-length index 1))
        rest-count (- distr-length index 1)
        most-blocks-count (inc index)
        first-blocks (map inc (range index))
        rest-blocks (take-last rest-count first-blocks)
        blocks (flatten (conj 
            (reverse first-blocks) 
            (inc index) 
            (if (pos? (count rest-blocks))
                rest-blocks
                [])))] 
        (if index-after-half? (reverse blocks) blocks)))

(defn weigh-distribution [distribution index] 
    (let [blocks (prob-blocks (count distribution) index)
        block-count (reduce + blocks)]
    (map #(/ % block-count) blocks)))
    
(def gen-note [(gen-pitch) (gen-duration)])

(defn add-notes [notes n]
    (if (zero? n) 
        notes 
        (add-notes (conj notes gen-note) (dec n))))
    
(defn -main [& args]
        (println "Compojer v0.1")
        (println (add-notes notes 10)))
