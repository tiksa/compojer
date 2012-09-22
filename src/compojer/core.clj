(ns compojer.core
    (:gen-class)
    (:import [javax.sound.midi MidiSystem Sequence MidiEvent ShortMessage]))
    
(def notes [])

(def pitch-symbols ['c1 'd1 'e1 'f1 'g1 'a1 'hb1 'c2 'd2 'e2])
(def duration-symbols ['1/8 '1/4 '1/2])

(def midi-pitch-map {'c1 70 'd1 72 'e1 74 'f1 75 'g1 77 'a1 79 'hb1 80 'c2 82 'd2 84 'e2 86})

(def available-pitches (range (count pitch-symbols)))
(def available-durations (range (count duration-symbols)))

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
	    (recur
            (rest distribution)
            (conj current (first distribution)))
	    (recur
            (rest distribution)
            (conj current (+ (first distribution) (last current)))))))

(defn cumulate [distribution]
    (cumulate-recur distribution []))
    
(defn gen-note-attr [weighed-distr]
    (let [random (rand)]
	(dec (count (filter #(< random %) (cumulate weighed-distr))))))   

(defn gen-note [last-note]
    [(gen-note-attr (weigh-distribution pitch-probs (first last-note)))
     (gen-note-attr (weigh-distribution duration-probs (second last-note)))])

(defn add-notes [notes n]
    (if (zero? n) 
        notes 
        (add-notes (conj notes (gen-note
            (if (empty? notes)
                [4 2]
				(last notes)))) (dec n))))
                
(defn nums-to-symbols [notes]
    (if (zero? (count notes))
        notes
        (cons [(nth pitch-symbols (first (first notes))) (nth duration-symbols (second (first notes)))]
            (nums-to-symbols (rest notes)))))
          
(defn close-track [track]
    (let [stop-msg (ShortMessage.)]
        (.setMessage stop-msg ShortMessage/STOP)
        (.add track (MidiEvent. stop-msg 100))))

(defn play-track-sequence [sq]
    (doto (MidiSystem/getSequencer)
        (.open)
        (.setSequence sq)
        (.start)))

(defn pitch-to-midi-pitch [pitch-symbol]
    (get midi-pitch-map pitch-symbol))

(defn get-pitch [note] (first note))

(defn create-msg [note]
    (let [msg (ShortMessage.)]
        (.setMessage msg ShortMessage/NOTE_ON (pitch-to-midi-pitch (get-pitch note)) 64)
        msg))

(defn notes-to-sounds-recur [notes sounds]
    (if (empty? notes)
        sounds
        (recur 
            (rest notes) 
            (cons 
                (MidiEvent. (create-msg (first notes)) (inc (count sounds))) 
                sounds))))

(defn notes-to-sounds [notes]
    (notes-to-sounds-recur notes []))

(defn add-sounds [track sounds] 
    (if (empty? sounds)
        nil
        (do (.add track (first sounds))
            (add-sounds track (rest sounds)))))  

(defn play-notes [notes]
    (let [sq (Sequence. Sequence/PPQ 2)
        track (.createTrack sq)]
        (add-sounds track (notes-to-sounds notes))
        (close-track track)
        (play-track-sequence sq)))

(defn -main [& args]
        (println "Compojer v0.1")
        (println "How long piece shall I compose? (notes)")
        (let [num-of-notes (Integer. (read-line))
              symbol-notes (nums-to-symbols (add-notes notes num-of-notes))]
            (println "Playing: " symbol-notes)
            (play-notes symbol-notes)))
