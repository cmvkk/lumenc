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

(defn write-data 
  "Helper function to render, it writes wave data to a given file output stream."
  [bos form fstart fend]
  (let [chunk-size (int (Math/ceil (/ (- fend fstart) *num-chunks*)))
	bb (java.nio.ByteBuffer/allocate (* 4 chunk-size))]
    (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)
    (loop [cur-chunk 0 start 0 end (min chunk-size fend)]
      (doseq [cur (range start end)]
	(.putInt bb (int (unchecked-multiply (int (form cur)) 64))))
      (.write bos (.array bb) 0 (* 4 chunk-size))
      (.position bb 0)
      (when (< cur-chunk *num-chunks*)
	(recur (inc cur-chunk) (+ start chunk-size) (min (+ end chunk-size) fend))))))

(defn render
  "Renders a wave into an actual .wav file."
  [[path end] form]
  (let [file (new java.io.File path)
        fos (new java.io.FileOutputStream file)
        bos (new java.io.BufferedOutputStream fos)]
    (println "Writing" end "samples to" file)
    (.write bos (get-header 0 end) 0 44)
    (write-data bos form 0 end)
    (.flush bos)
    (.close bos)))

;; OPERATORS

(defmacro wave
  "Creates a wave.  Takes a body of clojure code, and the variable s is provided, referring
   to the current sample number."
  [& forms]
  `(let [~'prev-sample (atom [-1 0])]
     (fn [~'s]
       (let [[sn# vl#] (deref ~'prev-sample)]
	 (if (= ~'s sn#)
	   vl#
	   (let [res# (do ~@forms)]
	     (swap! ~'prev-sample (constantly [~'s res#]))
	     res#))))))

(defmacro cwave
  "Returns a 'cached wave'.  Works exactly like a normal wave, with a few exceptions.
   The first form in the body must be either a number or nil.  This number determines
   how many previous samples the wave will store.  If nil is given, the wave will cache
   everything.  The number can be a decimal, but will be rounded up to the nearest integer.
   The cached values of the wave are available in the wave itself via the 'prev' function, 
   for example (prev (dec s)) will return the immediately previous return value."
  [num & forms]
  `(let [cache# (ref [])
	 next# (ref 0)]
     (if ~num
       (let [num# (int (Math/ceil ~num))]
	 (fn ~'prev [s#]
	   (if (and (< s# (deref next#)) (>= s# (- (deref next#) num#))) ;if result is cached
	     (get (deref cache#) (mod s# num#))
	     (let [~'s s#
		   new# (do ~@forms)]
	       (when (= (deref next#) s#)
		 (dosync
		  (when (= (deref next#) s#)
		    (alter cache# assoc (int (mod s# num#)) new#)
		    (alter next# inc))))
	       new#))))

       (fn ~'prev [s#]
	 (if (< s# (deref next#))
	   (get (deref cache#) s#)
	   (let [~'s s#
		 new# (do ~@forms)]
	     (when (= (deref next#) s#)
	       (dosync
		(when (= (deref next#) s#)
		  (alter cache# conj new#)
		  (alter next# inc))))
	     new#))))))


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
       (wave
         (int (/ (+ ~@(map (fn [x] `(~x ~'s)) gens)) ~size))))))

(defmacro add
  "Takes a series of waves and adds them together.  Beware, the returned
   wave may have a far greater amplitude range than the waves passed to it."
  [& forms]
  (let [size (count forms)
        gens (take size (repeatedly gensym))]
    `(let ~(into [] (apply concat (map vector gens forms)))
       (wave
         (int (+ ~@(map (fn [x] `(~x ~'s)) gens)))))))

(defmacro rstack 
  "Like stack, takes a series of label/filter pairs, and binds each wave to the return
   value of its corresponding filter.  Unlike stack, any filter call can refer to any other
   labels in the series as arguments, allowing for mutual recursion.  Beware, this has a high
   potential for causing an infinite loop and blowing the stack.  In particular, remember to use
   the rdelay filter here rather than buffer or shift, and make sure every constituent filter is 
   called at least once without going through your delay, otherwise the result will fail in quiet 
   and mysterious ways."
  [& forms]
  (let [len (/ (count forms) 2)
	atomlist (gensym)]
    `(let ~(into []  (concat `(~atomlist (map (fn [x#] (atom nil)) (range ~len)))
		              (apply concat (map (fn [i] 
						   `(~(nth forms (* i 2)) 
						     (fn [~'s] ((deref (nth ~atomlist ~i)) ~'s))))
						 (range len)))))
    ~@(map (fn [i] 
	    `(swap! (nth ~atomlist ~i) (fn [x#] ~(nth forms (inc (* i 2)))))) 
	   (range len)))))

(defn track? 
  "Returns true if object is a track."
  [mtrk]
  (and (vector? mtrk)
       (= (:track (first mtrk)) true)))

(defmulti initial-pass (fn [trk] (:type (first trk))))

(defn initial-p 
  "Applies a default type to the track and calls initial-pass on it.  This is
   run during deftrack."
  [[mp & trk]]
  (initial-pass (into [] (cons (if (:type mp) mp (assoc mp :type :note)) 
			       (if (= (first trk) [\. 0 0]) (rest trk) trk)))))

(defmulti final-pass (fn [trk] (:type (first trk))))

(defn final-p 
  "Changes beat values to sample values, and calls final-pass.  This is run
   when the track is changed into a wave or during with-track."
  [trk]
  (map (fn [[num s e]]
	 [num (beats s) (beats e)]) (next (final-pass trk))))


(defn finalized-track-wave
  "Takes an already finalized track and returns a wave that modulates based on its value."
  [ftrk]
  (wave
   (loop [[[val start end] & tail] ftrk]
     (if (and (>= s start) (<= s end))
       val
       (if tail
	 (recur tail)
	 0)))))

(defn track-wave
  "Takes a track and returns a wave that modulates based on its value."
  [trk]
  (finalized-track-wave (final-p trk)))

;; DEFFILTER

(defn get-filter-args
  "Helper function for deffilter.  Determines the arglist and the list of let bindings
   for the expanded defn."
  [args arglist letlist]
  (let [[arg & tail] args
	new-arglist (conj arglist (if (list? arg) (first arg) arg))
	new-letlist (if (list? arg)
		      letlist
		      (concat letlist `(~arg (if (fn? ~arg) 
					       ~arg 
					       (if (track? ~arg)
						 (track-wave ~arg)
						 (constantly ~arg))))))]
    (if tail
      (recur tail new-arglist           new-letlist)
      [(into []   new-arglist) (into [] new-letlist)] ))) 

(defn get-filter-body
  "Helpfer function for deffilter.  Takes an arglist and a set of forms and returns an
   expanded version with the appropriate let bindings."
  [args & forms]
  (let [[arglist letlist] (if (first args) (get-filter-args args [] []) [args args])]
    `(~arglist
      (let ~letlist
	~@forms))))

(defn get-filter-stuff
  "Helpfer function for deffilter.  Takes the 'body' of the filter (including the docstring)
   and returns an expanded version."
  [thing & stuff]
  (if (string? thing)
    `(~thing ~@(apply get-filter-stuff stuff))
    (if (list? thing)
      (if stuff
	`(~(apply get-filter-body thing) ~@(apply get-filter-stuff stuff))
	`(~(apply get-filter-body thing)))
      (get-filter-body thing (first stuff)))))

(defmacro deffilter
  "Defines a filter.  Works exactly like defn, with the caveat that arguments are somewhat 
   tampered with.  Namely, any argument passed to deffilter that isn't already a wave will
   be turned into one (by wrapping it in a call to constantly).  However 'static' arguments
   can be defined by wrapping the arg in a list.  For example, (deffilter foo [bar (baz)] ...)
   will turn bar into a wave if it isn't already one, but leave baz alone."
  [name & stuff]
  `(defn ~name ~@(apply get-filter-stuff stuff)))

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

(defn with-track-fn 
  "Helper function for with-track.  See with-track."
  [trks func] 
  (let [ntrk (get-total-track (map final-p trks) func)]
    (wave
     (loop [[[wav start end] & tail] ntrk]
       (if (and (>= s start) (<= s end))
	 (wav (- s start))
	 (if tail
	   (recur tail)
	   0))))))

(defmacro with-track 
  "The with-track form.  Takes a vector of Vars (bound to tracks) and
   a form they apply to that's expected to return a wave, and it produces
   a wave based on the tracks application to that form.  A better explanation
   should be available in the docs..."
  [trks & form]
  `(with-track-fn ~trks (fn ~trks ~@form)))
