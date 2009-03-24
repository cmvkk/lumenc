;; lumenc/main.clj
;; copyright 2009 William D. Lipe (cmvkk)


(ns lumenc.main
  (:refer-clojure)
  (:import java.lang.Math))

;; CONSTANTS

(def *rate* 44100)
(def *bpm* 120)
(def *num-chunks* 25)
(def *max-amp* 33554431) ; currently 2^25-1.  The actual intermediate
                         ; amplitudes are stored as signed 4-byte integers, making it possible
                         ; to write filters with a net gain of 64 before having to worry about
                         ; arithmetic overflow.

;; HELPER FNS

(defn secs 
  "Uses the Var *rate* to convert seconds to samples."
  [time]
  (int (* *rate* time)))

(defn beats 
  "Returns the number of samples for a length of time in beats, given *bpm* and *rate*."
  [time]
  (int (* *rate* time (/ 60 *bpm*))))

(defmacro with-fraction 
  "Allows you to call a wave with a fractional sample number, for example
   (with-fraction wav 2.5).  Uses linear interpolation to estimate a result
   given neighboring samples."
  [filt s]
  `(if (integer? ~s)
     (~filt ~s)
     (let [high# (int (Math/ceil ~s))
	   low# (int (Math/floor ~s))
	   frac# (mod ~s 1)]
       (+ (* frac# (~filt high#))
	  (* (- 1 frac#) (~filt low#))))))

(defmacro with-bpm [bpm & forms]
  `(binding [~'*bpm* ~bpm]
     ~@forms))

(defmacro hold []
  `((deref ~'prev-sample) 1))

;; RENDERER

(defn get-header 
  "Given a start and an end in samples, returns a byte array containing a valid wav file header."
  [start end]
  (let [csize (* 4 (- end start))
        fsize (+ 36 csize)]
    (.array (doto (java.nio.ByteBuffer/allocate 44)
	      (.order java.nio.ByteOrder/LITTLE_ENDIAN)
	      (.put (.getBytes "RIFF"))                                
	      (.putInt fsize)                                 ;file size (everything but the first eight bytes)
	      (.put (.getBytes "WAVE"))     
	      (.put (.getBytes "fmt "))
	      (.putInt 16)                                    ;chunk size (always 16 bytes)
	      (.putShort 1)                                   ;compression code (1 - uncompressed PCM)
	      (.putShort 1)                                   ;channels (1 - mono)
	      (.putInt *rate*)                                ;sample rate
	      (.putInt (* *rate* 4))                          ;bytes per second (at 4 bytes per sample)
	      (.putShort 4)                                   ;bytes per sample
	      (.putShort 32)                                  ;bits per sample point
	      (.put (.getBytes "data"))
	      (.putInt csize)))))                              ;chunk size


(defn write-chunks
  [#^java.io.BufferedOutputStream bos #^java.nio.ByteBuffer bb form end chunk-size]
  (let [csize (min chunk-size end)
	rform (loop [nf form cs csize]
		(.putInt bb (int (unchecked-multiply (int (first nf)) 64)))
		(if (> cs 1)
		  (recur (next nf) (dec cs))
		  (next nf)))]
    (.write bos (.array bb) 0 (* 4 chunk-size))
    (.position bb 0)
    (print "=") (flush)
    (when (> (- end csize) 0)
      (recur bos bb rform (- end csize) chunk-size))))

(defn write-data
  [bos form start end]
  (let [chunk-size (int (Math/ceil (/ (- end start) *num-chunks*)))
	bb (java.nio.ByteBuffer/allocate (* 4 chunk-size))]
    (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)
    (write-chunks bos bb form end chunk-size)))
  
(defn render
  "Renders a wave into an actual .wav file."
  [[path end] form]
  (let [file (new java.io.File path)
        fos (new java.io.FileOutputStream file)
        bos (new java.io.BufferedOutputStream fos)]
    (println "Writing" end "samples to" file)
    (println "----|----|----|----|----|")
    (.write bos (get-header 0 end) 0 44)
    (write-data bos form 0 end)
    (.flush bos)
    (.close bos)))

;; OPERATORS

(declare track?)
(declare track-wave)

(defn parse-wave-args [args]
  (if (empty? args)
    [[] [] [] []]
    (loop [waves []  ;list of waves
	   rests []  ;gensyms that will refer to the waves
	   fargs []  ;args for the fn that produces the lazy seq
	   inits []  ;initial values for the fn args (not including the waves)
	   [arg & tail] args 
	   inand false]
      (cond 
       (= arg :and) 
       (recur waves rests fargs inits tail true)
       
       (not inand) ; 'arg' is a wave
       (let [garg (gensym)
	     nwaves (conj waves arg)
	     nrests (conj rests garg)
	     nfargs (conj fargs [arg ':as garg])]
	 (if tail
	   (recur nwaves nrests nfargs inits tail inand)
	   [nwaves nrests nfargs inits]))
       
       true  ; 'arg' and '(first tail)' are a loop-style binding
       (let [nfargs (conj fargs arg)
	     ninits (conj inits (first tail))]
	 (if (next tail)
	   (recur waves rests nfargs ninits (next tail) inand)
	   [waves rests nfargs ninits]))))))

(defn wave-opts [waves]
  (map (fn [wav] `(cond 
                    (seq? ~wav) ~wav
                    (track? ~wav) (track-wave ~wav)
		    (instance? clojure.lang.Atom ~wav) (lazy-seq (seq (deref ~wav)))
                    true (repeat ~wav)))
       waves))

(defn expand-give [[give ret & args] rests myfn]
  (if (not (= give 'give))
    (throw Exception)
    `(cons ~ret (~myfn ~@(map (fn [r] `(if (next ~r) (next ~r) (cons 0 (repeat 0)))) rests) ~@args))))

(defn find-give [body rests myfn]
  (if (not (seq? body))
    body
    (if (= (first body) 'give)
      (expand-give body rests myfn)
      (map #(find-give % rests myfn) body))))

(defmacro wave [args & body]
  (let [[waves rests fargs inits] (parse-wave-args args)
        myfn (gensym)]
    `(let [~myfn (fn ~myfn ~(into [] fargs)
                   (lazy-seq
                     ~@(map #(find-give % rests myfn) body)))]
       (~myfn ~@(wave-opts waves) ~@inits))))

(declare give)

(defmacro stack
  "Takes a series of label/filter pairs, binding the return value of each filter to the corresponding
   label in order, just as let would do, and returning the value of the bottom filter.  The final 
   filter doesn't need a corresponding label."
  [& forms]
  (if (even? (count forms))
    `(let [~@forms] ~(last (butlast forms)))
    `(let [~@(butlast forms)] ~(last forms))))

(defmacro mix
  "Takes a series of waves and averages them together."
  [& forms]
  (let [size (count forms)
        gens (take size (repeatedly gensym))]
    `(let ~(into [] (apply concat (map vector gens forms)))
       (wave ~(into [] gens)
         (~'give (int (/ (+ ~@gens) ~size)))))))

(defmacro add
  "Takes a series of waves and adds them together.  Beware of
   arithmetic overflow."
  [& forms]
  (let [size (count forms)
	gens (take size (repeatedly gensym))]
    `(let ~(into [] (apply concat (map vector gens forms)))
       (wave ~(into [] gens)
	 (~'give (int (+ ~@gens)))))))

(defmacro rstack
  "Add this"
  [& forms]
  (let [len (/ (count forms) 2)
	atomlist (gensym)]
    `(let ~(into [] (concat `(~atomlist (take ~len (repeatedly #(atom nil))))
			     (apply concat (map (fn [i]
						  `(~(nth forms (* i 2))
						    (lazy-seq (cons 0 (deref (nth ~atomlist ~i))))))
						(range len)))))
       ~@(map (fn [i]
		`(reset! (nth ~atomlist ~i) (lazy-seq (seq ~(nth forms (inc (* i 2))))))) 
	      (range len)))))

		
(defn track? 
  "Returns true if object is a track."
  [mtrk]
  (and (vector? mtrk)
       (= (:track (first mtrk)) true)))

(defmulti initial-pass (fn [trk typ] typ))

(defn initial-p
  "Calls the initial pass method for each of the tracks types.  Also removes
   a possible 'blank frame' from the beginning of the track, and assigns it a default
   type if one doesn't already exist.  This is intended to be run during deftrack."
  [[mp & trk]]
  (let [nmp (if (:type mp) mp (assoc mp :type [:note]))]
    (loop [ntrk (cons mp (if (= (first trk) [\. 0 0]) (rest trk) trk)) 
	   tvec (:type nmp)]
      (if tvec
	(recur (initial-pass ntrk (first tvec)) (next tvec))
	ntrk))))

(defmulti final-pass (fn [trk typ] typ))


(defn final-p
  "Calls the final pass method for each of the tracks types.  Also converts beat lengths
   to sample lengths, and removes the no-longer-necessary map from the beginning.  This
   is intended to be run during with-track, or when passing a track to a filter."
  [[mp & trk]]
  (loop [ntrk (cons mp trk)
	 tvec (:type mp)]
    (if tvec
      (recur (final-pass ntrk (first tvec)) (next tvec))
      (map (fn [[num s e]]
	     [num (beats s) (beats e)]) (next ntrk)))))


(defn finalized-track-wave
  [ftrk]
  (wave [:and ctrk ftrk s 0]
    (loop [[[val start end] & tail :as ctrk] ctrk]
      (if (and (>= s start) (<= s end))
	(give val ctrk (inc s))
	(if tail
	  (recur tail)
	  (give 0 [[0 0 0]] s))))))


(defn track-wave
  "Takes a track and returns a wave that modulates based on its value."
  [trk]
  (finalized-track-wave (final-p trk)))


(defmacro deffilter [& stuff]
  `(defn ~@stuff))


;; DEFTRACK

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
      (let [inner-res (flatten-track (first trk) [(or (last res) [\. 0])] (/ note-len (count (first trk))))]
	(recur (next trk) (apply conj (if (empty? res) res (pop res)) inner-res) note-len))
      (if (= (first trk) '*)
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
		    [name (initial-p (into [] (cons (assoc fmap :track true) nvec)))])) 
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

;; WITH-TRACK

(defn split-track 
  "Splits a track at the given sample number.  Track 1 ends at sn, Track 2 starts at sn+1."
  [trk sn]
  (if (not (seq trk))
    [[[0 sn (inc sn)]] []]
    (loop [track1 [] [c start end] (first trk) track2 (next trk)]
      (if (and (>= sn start) (<= sn end)) ; splitting at curframe.
	(if (= sn end) ; even split
	  [(into [] (concat track1 [[c start end]]))                                     track2]
	  [(into [] (concat track1 [[c start  sn]])) (into [] (concat [[c (inc sn) end]] track2))])
	(if track2 ; not at the end
	  (recur (concat track1 [[c start end]]) (first track2) (next track2))
	  [(into [] (concat track1 [[c start end]])) nil])))))

(defn set-to-zero
  "Takes a track and sets it to zero."
  [trk]
  (let [sval ((first trk) 2)]
    (into [] (map (fn [[v s e]]
		    [v (- s sval) (- e sval)]) trk))))

(defn get-next-track-val
  "Takes a track, splits it up at sn, and then returns a value
   for the first part of the track (either a constant value if
   the first part only contains one note, or a wave modulating
   on the different notes) plus the rest of the track."
  [trk sn]
  (let [[b a] (split-track trk sn)]
    (if (= (count b) 1)
      [(first (first b)) a]
      [(finalized-track-wave (set-to-zero b)) a])))

(defn get-total-track
  "Helper function for with-track.  Takes a list of tracks and 
   a function, and splits up the tracks based on the frames in the
   first track, calling the function with the values produced each time
   and swapping in the resultant wave."
  [trks func]
  (let [[mtrk & rtrks] trks]
    (loop [[[val start end] & tail] (first trks) rtrks (next trks) res []] 
      (let [firsts (map #(first (get-next-track-val % end)) rtrks)
	    seconds (map #(second (get-next-track-val % end)) rtrks)]
	(if tail
	  (recur tail seconds (concat res [[(apply func (cons val firsts)) start end]]))
	  (into [] (concat res [[(apply func (cons val firsts)) start end]])))))))

;(defn with-track-fn 
;  "Helper function for with-track.  See with-track."
;  [trks func] 
;  (let [ntrk (get-total-track (map final-p trks) func)]
;    (wave [:and ctrk ntrk s 0 cwav ]
;     (loop [[[wav start end] & tail :as ctrk] ctrk]
;       (if (and (>= s start) (<= s end))
;	 (give wav ctrk (inc s))
;	 (if tail
;	   (recur tail)
;	   (give 0 [[0 0 0]] s)))))))

(defn with-track-fn
  [trks func]
  (let [ntrk (get-total-track (map final-p trks) func)]
    (mapcat (fn [[wav start end]]
	      (take (- end start) wav))
	    ntrk)))

(defmacro with-track 
  "The with-track form.  Takes a vector of Vars (bound to tracks) and
   a form they apply to that's expected to return a wave, and it produces
   a wave based on the tracks application to that form.  A better explanation
   should be available in the docs..."
  [trks & form]
  `(with-track-fn ~trks (fn ~trks ~@form)))
