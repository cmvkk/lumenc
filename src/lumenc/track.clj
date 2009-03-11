;; lumenc/track.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.track
  (:refer-clojure)
  (:use lumenc.main))


;; NOTE TRACK

;;      c       c#      d       d#      e       f       f#      g       g#      a       a#      b
(def *chromatic* [
        0       0       0       0       0       0       0       0       0       0       0       0
       16.35   17.32   18.35   19.45   20.60   21.83   23.13   24.5    25.96   27.5    29.14   30.87     ;; 0
       32.70   34.65   36.71   38.89   41.20   43.65   46.25   49      51.91   55      58.27   61.74     ;; 1
       65.41   69.3    73.42   77.78   82.41   87.31   92.50   98     103.83  110     116.54  123.47     ;; 2
      130.81  138.59  146.83  155.56  164.81  174.61  185     196     207.65  220     233.08  246.94     ;; 3
      261.63  277.18  293.66  311.13  329.63  349.23  369.99  392     415.3   440     466.16  493.88     ;; 4 
      523.25  554.37  587.33  622.25  659.26  698.46  739.99  783.99  830.61  880     932.33  987.77     ;; 5
     1046.5  1108.73 1174.66 1244.51 1318.51 1396.91 1479.98 1567.98 1661.22 1760    1864.66 1975.33     ;; 6
     2093    2217.46 2349.32 2489.02 2637.02 2793.83 2959.96 3135.96 3322.44 3520    3729.31 3951.07     ;; 7
     4186.01 4434.92 4698.64 4978.03 5274.04 5587.65 5919.91 6271.93 6644.88 7040    7458.62 7902.13])   ;; 8


(def *note-nums* {\c 0 \d 2 \e 4 \f 5 \g 7 \a 9 \b 11})

(defn get-note 
  "Given a note symbol, plus a set of options and a current octave, returns the note number for that note."
  [note opts oct]
  (let [note (seq (str note))]
    (if (= (first note) \.)
      [\. oct]
      (let [cur-oct (if-let [moct (first (filter #(Character/isDigit %) note))]
		      (Character/digit moct 10)
		      oct)
	    alter (cond
		   (contains? (set (next note)) \b) -1
		   (contains? (set (next note)) \#) 1
		   (contains? (set (next note)) \n) 0
		   (contains? (set (:flats opts)) (first note)) -1
		   (contains? (set (:sharps opts)) (first note)) 1
		   true 0)]
	[(+ 12 (* cur-oct 12) (*note-nums* (first note)) alter) cur-oct]))))

(defn get-notes 
  "Given a flattened track, takes the note symbols from the frames and replaces them with note numbers."
  [trk opts]
  (loop [[[note times timee] & tail] trk 
	 res []
	 oct (or (:oct opts) 4)]
    (let [[freq new-oct] (get-note note opts oct)
	  new-res (conj res [freq times timee])]
      (if tail
	(recur tail new-res new-oct)
	new-res))))


(defmethod initial-pass :note [[mp & trk]]
  (let [new-trk (get-notes trk mp)]
    (into [] (cons mp new-trk))))

(defmethod final-pass :note [[mp & trk]]
  (into [] (cons mp (map (fn [[note s e]]
			   [(if (not= \. note)
			      (*chromatic* note) 0) s e]) trk))))

;; RAW TRACK

(defmethod initial-pass :raw [trk]
  trk)

(defmethod final-pass :raw [trk]
  trk)



;; TRACK FNS

(defn transpose
  "Changes the track's key by num semitones."
  [num [mp & trk]]
  (into [] (cons mp
		 (map (fn [[val start end]]
			(if (= val \.)
			  [0 start end]
			  [(+ val num) start end])) trk))))


