(ns compojer.core (:gen-class))
    
(def notes [])

(def available-pitches (range 8))
(def available-durations (range 6))

(defn create-flat-distribution [length] (repeat length (/ 1 length)))

(def pitch-probs (create-flat-distribution (count available-pitches)))
(def duration-probs (create-flat-distribution (count available-durations)))

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

(defn cumulate-recur [distribution current]
    (if (zero? (count distribution))
	current
	(if (zero? (count current))
	    (cumulate-recur
		(rest distribution)
		(conj current (first distribution)))
	    (cumulate-recur
		(rest distribution)
		(conj current (+ (first distribution) (last current)))))))

(defn cumulate [distribution]
    (cumulate-recur distribution []))
    
(defn gen-pitch [last-pitch]
    (let [random (rand)
	  weighed-distr (weigh-distribution pitch-probs last-pitch)]
	(dec (count (filter #(< random %) (cumulate weighed-distr))))))
    
(defn gen-duration [last-duration]
    (let [random (rand)
	  weighed-distr (weigh-distribution duration-probs last-duration)]
	(dec (count (filter #(< random %) (cumulate weighed-distr))))))


(defn gen-note [last-note]
    [(gen-pitch (first last-note)) (gen-duration (second last-note))])

(defn add-notes [notes n]
    (if (zero? n) 
        notes 
        (add-notes (conj notes (gen-note
				   (if (empty? notes)
				       [3 3]
				       (last notes)))) (dec n))))
    
(defn -main [& args]
        (println "Compojer v0.1")
        (println (add-notes notes 10)))
