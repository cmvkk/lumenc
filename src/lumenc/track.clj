;; lumenc/track.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.track
  (:refer-clojure)
  (:use lumenc.main))

;; CONSTANTS


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

;; TRACK FNS

(defn beats 
  "Returns the number of samples for a length of time in beats, given *bpm* and *rate*."
  [time]
  (int (* *rate* time (/ 60 *bpm*))))

(defn get-note 
  "Given a note symbol, plus a set of options and a current octave, returns the note number for that note."
  [note opts oct]
  (let [note (seq (str note))]
    (if (= (first note) \r)
      [\r oct]
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

(defmulti get-track (fn [m t] (or (:type m) :note)))

(defmethod get-track :raw [mp trk]
  (concat [mp] trk))

(defmethod get-track :note [mp trk]
  (let [new-trk (get-notes trk mp)]
    (concat [mp] new-trk)))

(defn do-track-times 
  "The input track here has only lengths of time in its frames, but what we need is
   start time and end time.  Uses a running time count to calculate this."
  [trk]
  (loop [[[val len] & tail] trk
	 ntrk []
	 time 0]
    (if tail
      (recur tail (conj ntrk [val time (+ len time)]) (+ len time))
      (conj ntrk [val time (+ len time)]))))
    

(defn flatten-track
  "Given a track input, takes the track and flattens it, returns a vector of 'frames', which contain a note and the 
   length it is to be played, in beats."
  [trk res note-len]
  (if trk
    (if (list? (first trk))
      (let [inner-res (flatten-track (first trk) [] (/ note-len (count (first trk))))]
	(recur (next trk) (apply conj res inner-res) note-len))
      (if (= (first trk) 'h)
	(let [prev-note (last res)]
	  (recur (next trk) (conj (pop res) [(first prev-note) (+ note-len (second prev-note))]) note-len))
	(recur (next trk) (conj res [(first trk) note-len]) note-len)))
    res))


(defn refine-tracks 
  "Input here is a map of names connected to a vector of values that are associated with it.  This function
   joins the values into a full track, then passes the track to get 'flattened' and otherwise edited according
   to its type."
  [res]
  (into {} (map (fn [[name values]]
		  (let [maps (filter map? values)
			fmap (or (apply merge maps) {})
			vecs (filter vector? values)
			fvec (apply concat vecs)
			nvec (do-track-times (flatten-track fvec [] (or (:bpn fmap) 1)))]
		    [name (get-track (or fmap {}) nvec)])) 
		res)))
	   
(defn add-to-res 
  "Helper function to deftrack-fn, takes a 'chunk' of names and track pieces, and adds them to the result map."
  [res names data]
  (merge-with #(into [] (concat %1 %2)) 
	      res (reduce (fn [sofar name]
			    (merge-with concat sofar {name (into [] data)}))
			  {} names)))

(defn deftrack-fn 
  "Takes the list of names and values, and associates them with each other in a map."
  [res names data on-name forms]
  (if forms
    (if (symbol? (first forms))
      (if on-name
	(recur res (conj names (first forms)) data true (next forms))
	(recur (add-to-res res names data) [] [] true forms))
      (recur res names (conj data (first forms)) false (next forms)))
    (if (or names data)
      (add-to-res res names data)
      res)))

(defmacro deftrack 
  "The main macro.  Takes a list of name*/value* pairs and associates
   them together, binding all the associated values to the name. See
   documentation elsewhere for details..."
  [& forms]
  (let [res (refine-tracks (deftrack-fn {} [] [] true forms))]
    `(do
       ~@(map (fn [[name trk]]
		`(def ~name ~(into [] trk))) res))))

(defn transpose 
  "Given an offset and a track object, offsets the notes in the track by that many semitones."
  [num trk]
  (map (fn [cur]
	 (let [note (first cur)]
	   (assoc cur 0 (+ note num)))) (rest trk)))


;; FILTERS

(deffilter do-track 
  "Takes a partially-applied filter and a track object, and applies the track value for each
   note to the filter at the right times, creating a wave of the filter playing that melody."
  [wav (trk)]
  (let [new-trk (map (fn [[num s e]] [(wav (*chromatic* num)) (beats s) (beats e)]) trk)]
    (wave
     (loop [[[cwav start end] & tail] new-trk]
       (if (and (>= s start) (<= s end))
	 (cwav (- s start))
	 (if tail
	   (recur tail)
	   0))))))


(deffilter do-tracks
  "Takes a partially-applied filter and a collection of track objects, and mixes the resultant
   track waves together using do-track."
  [wav (trks)]
  (let [wavs (map #(do-track wav %) trks)
	size (count trks)]
    (wave
     (/ (reduce + (map #(% s) wavs)) size))))

