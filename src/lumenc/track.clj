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

(defn get-note-direction
  [note]
  (let [plus-count (count (filter (set (list \+)) note))
	minus-count (count (filter (set (list \-)) note))]
    (- plus-count minus-count)))

(defn get-note
  [val mp last-val]
  (let [[letr & note] (seq (str val)) ; letr is the note letter, note is the extra stuff
	moct (first (filter #(Character/isDigit %) note)) ;get an octave if one exists
	offset (cond
		(contains? (set note) \b) -1
		(contains? (set note) \#) +1
		(contains? (set note) \n)  0
		(contains? (set (:flats  mp)) letr) -1
		(contains? (set (:sharps mp)) letr) +1
		true 0)]
    (if moct
      (+ (* moct 12) (*note-nums* letr) offset) ;if an octave exists, use it
      (if (not last-val)
	(+ (* (or (:oct mp) 4) 12) (*note-nums* letr) offset) ;if no last value, we use the default octave
	(let [possible-nums (map #(+ (* % 12) (*note-nums* letr) offset) (range 8))
	      direction (get-note-direction note)]
	  (cond 
	   (= direction 0)
	   (reduce (fn [x y]                           ; direction is 0, so we use note closest to last-val   
		     (if (< (Math/abs (- last-val x)) 
			    (Math/abs (- last-val y))) 
		       x y))
		   possible-nums)
	    (< direction 0)                            ; grab the nth possible val greater/lesser than the last val
	    (nth (reverse (filter #(< % last-val) possible-nums)) (dec (- direction)))
	    (> direction 0)
	    (nth          (filter #(> % last-val) possible-nums)  (dec direction))))))))
	    	 
(defn get-notes
  [trk last-val]
  (lazy-seq
   (if (not (first trk))
     nil
     (let [[val len mp] (first trk)
	   new-val (get-note val mp last-val)]
       (cons [new-val len mp] (get-notes (rest trk) new-val))))))
	   
(defmethod initial-pass :note 
  [trk typ]
  (get-notes trk nil))

(defmethod final-pass :note
  [[val len mp] typ]
  [(*chromatic* val) len mp])


;; ARPEGGIO TRACK

(defmethod initial-pass :arpeggio
  [trk typ]
  (mapcat (fn [[val len mp :as frame]]
	    (if (vector? val)
	      (lazy-loop [cura 0
			  left len]
		(let [clen (min left (:alen mp))
		      cnote (val cura)]
		  (if (> (- left clen) 0)
		    (give [cnote clen mp] 
			  (mod (inc cura) (count val)) 
			  (- left clen))
		    (list [cnote clen mp]))))
	      [frame]))
	  trk))


(defmethod final-pass :arpeggio 
  [frame typ]
  frame)

;; OPTION TRACK

(defmethod initial-pass :option 
  [trk typ]
  trk)

(defmethod final-pass :option
  [[val len mp] typ]
  [((:opts mp) val) len mp])


;; RAW TRACK

(defmethod initial-pass :raw 
  [trk typ]
  trk)

(defmethod final-pass :raw 
  [frame typ]
  frame)



;; TRACK FNS

(defn transpose
  "Changes the track's key by num semitones."
  [num trk]
  (map (fn [[val len mp]]
	 (if (= val \.)
	   [0 len mp]
	   [(+ val num) len mp]))
       trk))

