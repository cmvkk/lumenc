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

(defn get-filter-args
  "Helper function for deffilter.  Determines the arglist and the list of let bindings
   for the expanded defn."
  [args arglist letlist]
  (let [[arg & tail] args
	new-arglist (conj arglist (if (list? arg) (first arg) arg))
	new-letlist (if (list? arg)
		      letlist
		      (concat letlist `(~arg (if (fn? ~arg) ~arg (constantly ~arg)))))]
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
   and mysterious ways.  This is also the only operator that relies on a filter to work, the 'express'
   filter which is defined in cmvkk.wav.standards, so you have to have loaded that in order for this
   to work."
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

;; HELPER FNS


(defn secs 
  "Uses the Var *rate* to convert seconds to samples."
  [time]
  (int (* *rate* time)))

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

(defmacro hold []
  `((deref ~'prev-sample) 1))