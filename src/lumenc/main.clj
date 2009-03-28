;; lumenc/main.clj
;; copyright 2009 William D. Lipe (cmvkk)


(ns lumenc.main
  (:refer-clojure)
  (:import java.lang.Math))

;; CONSTANTS

(def *rate* 44100)
(def *bpm* 120)
(def *num-chunks* 80)
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

(defmacro give [ret & args]
  `(cons ~ret (~'llfn ~@args)))

(defmacro lazy-loop [args & body]
  (let [fargs (map first (partition 2 args))
        inits (map second (partition 2 args))]
    `(let [~'llfn (fn ~'llfn ~(into [] fargs)
                    (lazy-seq
                      ~@body))]
       (~'llfn ~@inits))))

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

(defmacro render
  "Renders a wave into an actual .wav file."
  [[path end] form]
  `(let [file# (new java.io.File ~path)
	 fos# (new java.io.FileOutputStream file#)
	 bos# (new java.io.BufferedOutputStream fos#)
	 end# ~end
	 ntime# (java.lang.System/nanoTime)]
     (printf "Writing %d samples (%.2f seconds) to <%s>" end# (float (/ end# *rate*)) ~path)
     (println "\n         |         |25%      |         |50%      |         |75%      |         |")
     (println   "----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|")
     (.write bos# (get-header 0 end#) 0 44)
     (let [chunk-size# (int (Math/ceil (/ end# *num-chunks*)))
	   bb# (java.nio.ByteBuffer/allocate (* 4 chunk-size#))]
       (.order bb# java.nio.ByteOrder/LITTLE_ENDIAN)
       (write-chunks bos# bb# ~form end# chunk-size#))
     (let [etime# (java.lang.System/nanoTime)
	   ttime# (- etime# ntime#)
	   real-nsps# (/ ttime# end#)
	   want-nsps# (/ 1000000000 *rate*)
	   rat# (/ want-nsps# real-nsps#)]
       (printf "\nIn %.2f seconds (%.2fx real time)." (/ ttime# 1000000000.0) (float rat#))
       (println "\n"))
     (.flush bos#)
     (.close bos#)))

;; OPERATORS

(declare track?)
(declare track-wave)

(defn parse-wave-args 
  "Takes the arguments for 'wave', and splits them up as necessary to be used in the different
   places of the macro expansion."
  [args]
  (if (empty? args)
    [[] [] [] []]
    (loop [waves []  ;list of waves
	   rests []  ;gensyms that will refer to the waves
	   fargs []  ;args for the fn that produces the lazy seq
	   binds []  ;bindings for the let inside the lazy-seq
	   inits []  ;initial values for the fn args (not including the waves)
	   [arg & tail] args 
	   inand false]
      (cond 
       (= arg :and) 
       (recur waves rests fargs binds inits tail true)
       
       (not inand) ; 'arg' is a wave
       (let [garg (gensym)
	     nwaves (conj waves arg)
	     nrests (conj rests garg)
	     nbinds (concat binds [arg `(or (first ~garg) 0)])
	     nfargs (conj fargs garg)]
	 (if tail
	   (recur nwaves nrests nfargs nbinds inits tail inand)
	   [nwaves nrests nfargs nbinds inits]))
       
       true  ; 'arg' and '(first tail)' are a loop-style binding
       (let [nfargs (conj fargs arg)
	     ninits (conj inits (first tail))]
	 (if (next tail)
	   (recur waves rests nfargs binds ninits (next tail) inand)
	   [waves rests nfargs binds ninits]))))))

(defn wave-opts 
  "Expands the code that coerces various input values of 'wave' into lazy-seq form."
  [waves]
  (map (fn [wav] `(cond 
		    (track? ~wav) (track-wave ~wav) 
                    (seq? ~wav) ~wav
                    true (repeat ~wav)))
       waves))

(defn expand-give 
  "Manually expands the 'give' call into actual code."
  [[give ret & args] rests myfn]
  (if (not (= give 'give))
    (throw Exception)
    `(cons ~ret (~myfn ~@(map (fn [r] `(rest ~r)) rests) ~@args))))

(defn find-give 
  "Helper function for wave, this traverses through the body of wave looking for 
   a sequence that starts with the symbol 'give, then calls expand-give on it."
  [body rests myfn]
  (if (not (seq? body))
    body
    (if (= (first body) 'give)
      (expand-give body rests myfn)
      (map #(find-give % rests myfn) body))))

(defmacro wave 
  "Takes a set of arguments and a body.  The arguments take the form of
   [waves* :and bindings*] where waves are input waves (must be symbols), 
   and bindings are let-like bindings of anything.  The body is called for every
   element of the resultant wave, and for each iteration, the symbol for each
   input wave is bound to the current element for that wave.  You can then use
   the function 'give' to return.  'give's first argument is a return value for
   that element in the wave, and the other arguments are for rebinding the initial
   bindings, a la recur."
  [args & body]
  (let [[waves rests fargs binds inits] (parse-wave-args args)
        myfn (gensym)]
    `(let [~myfn (fn ~myfn ~(into [] fargs)
                   (lazy-seq
		    (let ~(into [] binds)
                     ~@(map #(find-give % rests myfn) body))))]
       (~myfn ~@(wave-opts waves) ~@inits))))

(defmacro deffilter 
  "Defines a filter.  Exactly the same syntax as defn."
  [& stuff]
  `(defn ~@stuff))


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
  "Takes a series of let-like bindings, with a symbol paired to a form that returns a wave.
   The symbols can then be passed as arguments to each other's forms, producing mutually 
   recursive waves.  In order for this to work properly, one of the filters must be a 
   delay filter (i.e. 'buffer' or 'shift')."
  [& forms]
  (let [len (/ (count forms) 2)
	atomlist (gensym)]
    `(let ~(into [] (concat `(~atomlist (take ~len (repeatedly #(atom nil))))
			     (apply concat (map (fn [i]
						  `(~(nth forms (* i 2))
						    (lazy-seq (seq (deref (nth ~atomlist ~i))))))
						(range len)))))
       ~@(map (fn [i]
		`(reset! (nth ~atomlist ~i) ~(nth forms (inc (* i 2))))) 
	      (range len)))))


;; DEFTRACK

(defmulti initial-pass 
  "This method is run once for each type supplied for a track.  It takes a whole track object
   as well as a type keyword, and returns a new track object."
  (fn [trk typ] typ))

(defn initial-p 
  "Hooks the track into the initial-pass methods for each of its types.  Intended
   to be run before the track is returned from deftrack."
  [trk]
  (loop [types (:type ((first trk) 2))
	 ctrk trk]
    (if types
      (recur (next types) (initial-pass ctrk (first types)))
      ctrk)))

(defmulti final-pass 
  "This method is run once for each type supplied for a track frame.  It takes a single track
   frame as well as a type keyword, and returns a new track frame."
  (fn [frame typ] typ))

(defn final-p
  "Hooks the track into the final-pass methods for each of its types, and also
   changes beat lengths to samples.  Intended to be run just before the track is used in a wave."
  [trk]
  (map (fn [[val len mp :as frame]]
	 (loop [types (:type mp)
		cf frame]
	   (if types
	     (recur (next types) (final-pass cf (first types)))
	     cf)))
       (map (fn [[val len mp]] [val (beats len) mp]) trk)))

(defn collapse-holds
  "Takes a track object and removes holds, i.e. taking any frame with val '* and 
   removing it, adding its length to the immediately previous frame."
  [tseq]
  (lazy-seq
    (let [[val len mp] (first tseq)]
      (if (= val '*)
	(throw (new Exception "Can't start a track with a hold."))
	(if (not (next tseq))
	  (cons [val len mp] nil)
	  (loop [[[nval nlen nmp] :as nseq] (next tseq)
		 tlen len]
	    (if (= nval '*)
	      (if (next nseq)
		(recur (rest nseq) (+ tlen nlen))
		(cons [val (+ tlen len) mp] nil))
	      (cons [val tlen mp] (collapse-holds nseq)))))))))

(defn flatten-track
  "Takes a vector and map from deftrack, and 'flattens' it into a lazy-seq
   of actual track frames."
  ([tmap tvec]
     (flatten-track tmap tvec nil))
  ([tmap tvec len]
     (let [len (or len (:bpn tmap) 1)]
       (mapcat (fn [val]
		 (if (seq? val)
		   (flatten-track tmap val (/ len (count val)))
		   [[val len tmap]]))
	       tvec))))

(defn refine-track
  "Takes a set of input values from deftrack and returns an actual track object."
  [values]
  (let [tmap (if-let [mp (apply merge (filter map? values))]
	       (if (:type mp)
		 mp
		 (assoc mp :type [:note]))
	       {:type [:note]})
	tvec (apply concat (filter vector? values))]
    (with-meta (initial-p (collapse-holds (flatten-track tmap tvec))) {:type :track})))

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
  "The main macro.  This takes a list of name*/value* pairs and associates
   them together, binding all the associated values to the name.  See
   documentation elsewhere for details..."
  [& forms]
  (let [res (deftrack-fn {} [] [] true forms)]
    `(do
       ~@(map (fn [[key val]]
		`(def ~key (refine-track (quote ~val))))
	      res))))


;; TRACK-WAVE

(defn track?
  "Returns true if object is a track."
  [mtrk]
  (= (:type ^mtrk) :track))

(defn finalized-track-wave
  [ftrk]
  (wave [:and ctrk ftrk s 1]
   (let [[[val len mp] & tail :as ctrk] ctrk]
     (if (< s len)
       (give val ctrk        (inc s))
       (give val (next ctrk) 1)))))

(defn track-wave
  "Takes a track and returns a wave that modulates based on its value."
  [trk]
  (finalized-track-wave (concat (final-p trk) (repeat [0 10000 {}]))))

;; WITH-TRACK

(defn split-track
  "Splits a track at a given length (in samples)."
  [trk sn]
  (loop [track1 []
	 [[val len mp] & tail :as track2] trk
	 s sn]
    (cond
     (= s len) [(with-meta (conj track1 [val s mp]) {:type :track}) (with-meta tail {:type :track})]
     (< s len) [(with-meta (conj track1 [val s mp]) {:type :track}) (with-meta (cons [val (- len s) mp] tail) {:type :track})]
     (> s len) (recur (conj track1 [val len mp]) tail (- s len)))))

(defn get-next-track-val
  "Takes a track, splits it up at sn, and then returns a value
   for the first part of the track (either a constant value if
   the first part only contains one note, or a wave modulating
   on the different notes) plus the rest of the track."
  [trk sn]
  (let [[b a] (split-track trk sn)]
    (if (= (count b) 1)
      [(first (first b)) a]
      [(finalized-track-wave b) a])))

(defn get-total-track
  "Given a series of tracks and a function, returns a track whose vals are waves representing
   the application of that function to the values of each track."
  [[[[val len mp] & tail] & rtrks] func]
  (lazy-seq
   (let [splits (map #(get-next-track-val % len) rtrks)
	 res [(apply func (cons val (map first splits))) len mp]]
     (cons res (get-total-track (cons tail (map second splits)) func)))))

(defn with-track-fn
  "Helper function for with-track.  Takes a sequence of tracks and a function and returns a
   wave, as per the with-track macro."
  [trks func]
  (let [ntrk (get-total-track (map #(concat (final-p %) (repeat [0 10000 {}])) trks) func)]
    (mapcat (fn [[val len mp]]
	      (take len val))
	    ntrk)))

(defmacro with-track 
  "The with-track form.  Takes a vector of Vars (bound to tracks) and
   a form they apply to that's expected to return a wave, and it produces
   a wave based on the tracks application to that form.  A better explanation
   should be available in the docs..."
  [trks & form]
  `(with-track-fn ~trks (fn ~trks ~@form)))