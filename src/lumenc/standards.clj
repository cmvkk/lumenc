;; lumenc/standards.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.standards
  (:refer-clojure)
  (:use lumenc.main))

;; UTILITY FILTERS

(deffilter pan 
  "Returns a wave that goes from 'start'
   to 'end' in 'len' samples."
  [(start) (end) (len)]
  (wave
    (+ (* end   (/ s         len)) 
       (* start (/ (- len s) len)))))


(deffilter parallel
  "Causes a the given wave's generation to be spun off
   into its own thread, making it concurrent."
  [wav (tsamps)]
  (let [#^java.util.concurrent.LinkedBlockingQueue lbq (new java.util.concurrent.LinkedBlockingQueue)
	 ag (agent nil)]
     (.add lbq 0)
     (send ag (fn [a]
		(try
		 (loop [s 0]
		   (.add lbq (wav s))
		   (when (< s tsamps)
		     (recur (inc s))))
		 (catch Exception e (.printStackTrace e)))))
     (fn [s]
       (.take lbq))))

(deffilter negate 
  "Negates a wave, flips it upsidown over 0."
  [wav]
  (wave
   (- (wav s))))

(deffilter period 
  "Converts a wave returning frequencies to one returning
   period lengths."
  [freq]
  (wave
   (/ *rate* (freq s))))

(deffilter cap 
  "'Caps' a wave after a certain time, returning only silence
   after that point."
  [wav (len)]
  (wave
   (if (> s len)
     0
     (wav s))))

(deffilter cache 
  "Caches a wave's prior return values for len samples, allowing
   the wave to be called with past values easily."
  [wav (len)]
  (cwave len
    (wav s)))

(deffilter express 
  "Stores a wave's last calculated return value, which can then
   be retrieved by calling the wave with the number -1.  This
   is mostly intended for use inside the rstack operator, in order
   to keep rdelay for inciting infinite recursion."
  [wav]
  (let [last (atom 0)]
    (wave
     (if (= s -1)
       @last
       (swap! last (fn [_] (wav s)))))))

(deffilter neg-safe 
  "When the wave is called with a negative sample number, returns
   0 instead of nil, which will keep the program from throwing
   an exception.  Don't use with 'express'."
  [wav]
  (wave
   (if (< s 0)
     0
     (wav s))))

;; DELAYS

(deffilter shift 
  "'Shifts' a wave forward or backward in time.  A positive len
   will delay the wave for that len samples, while a negative len
   will crop the front of the wave for len samples.  This is done
   in a cache-less manner, so it's safe to use with very large values
   of len without worrying about memory.  However, len must be a whole
   number."
  [wav (len)]
  (wave
   (if (< (- s len) 0)
     0
     (wav (- s len)))))

(deffilter buffer 
  "Delays a wave for len samples.  Size is the 'maximum delay' the buffer
   will be capable of, allowing it to define a static cache size.  len
   however can be a wave, allowing for shifts in delay length over time, 
   as long as len never exceeds size.  The delay is accomplished using an
   internal cache, so for large delays it might be better to use 'shift'.  
   Len can also be fractional, and size can be fractional but will be rounded
   up."
  [wav (size) len]
  (let [hold (atom (into [] (take size (repeat 0))))]
    (wave
     (let [new (if (< s 1) 0 (wav (dec s)))
	   ns (mod (- s (dec (len s))) size)
	   res (if (< s (len s)) 
		 0 (with-fraction #(get @hold (mod % size)) ns))]
       (swap! hold assoc (int (mod s size)) new)
       res))))

(deffilter rdelay
  "Like buffer, creates a delay using an internal cache.  It takes exactly
   the same inputs as buffer and behaves exactly the same way, except that
   it can only be used inside an rstack, in situations where its output
   is fed back into its input.  If used outside of an rstack, it
   will return only silence (probably).  Similarly, if buffer is used inside
   an rstack, it will probably cause a StackOverflowError."
  [wav (size) len]
  (let [hold (atom (into [] (take size (repeat 0))))]  ; ring buffer for delayed data
    (wave
     (let [new (if (< s 1) 0 (wav (dec s))) ; grab new sample 
	   ns (mod (- s (dec (len s))) size) ;calculate position of delayed sample in the buffer
	   res (if (< s (len s))
		 0 (with-fraction #(get @hold (mod % size)) ns))] ;uses linear interpolation to get sample
       (swap! hold assoc (int (mod s size)) new) ;add new sample to buffer
       res)))) ; return old sample

;; BASE WAVEFORMS

(deffilter sine 
  "Returns a sine wave.  Freq can be a wave, allowing for frequency changes
   over time."
  [freq]
  (wave
    (int (* *max-amp* (Math/sin (* s (/ (* 2 Math/PI (freq s)) *rate*))))))) 

(deffilter pulse 
  "Returns a pulse wave.  Freq and duty can both be waves, allowing for their
   changes over time. Duty should be a number between 0 and 1."
  [freq duty]
  (wave
    (if (< (/ (rem s (/ *rate* (freq s))) (/ *rate* (freq s))) 
	   (duty s))
      *max-amp* (- *max-amp*))))

(deffilter square 
  "Returns a pulse wave with a duty fixed at 0.5."
  [freq]
  (pulse freq 0.5))

(deffilter triangle 
  "Returns a triangle wave.  Freq and duty can both be waves, allowing for their changes
   over time.  Duty should be a number between 0 and 1, with 0.5 being a pure triangle
   wave."
  [freq duty]
  (wave
    (if (< (rem (/ s (/ *rate* (freq s))) 1) (duty s))
      (* *max-amp* (- 1 (/      (rem (/ s (/ *rate* (freq s))) 1)       (duty s)  0.5)))
      (* *max-amp* (- 1 (/ (- 1 (rem (/ s (/ *rate* (freq s))) 1)) (- 1 (duty s)) 0.5))))))

(deffilter sawtooth 
  "Returns a triangle wave with the duty fixed at 1."
  [freq]
  (triangle freq 1))

(deffilter white-noise 
  "Returns random data, cached to make it deterministic.  Freq indicates the rate
   at which the data will be repeated, use nil for data that never repeats."
  [(freq)]
  (let [random (new java.util.Random)
	size (int (Math/ceil (/ *rate* freq)))]
    (cwave size
     (if (< s size)
       (unchecked-divide (.nextInt random) 256) ; 4 to 3 bytes
       (prev (mod s size))))))

;; VOLUME FILTERS

(deffilter gain 
  "Returns a wave multiplied by a certain gain.  The gain can be a wave,
   allowing for gain changes over time."
  [wav mul]
  (wave
    (int (* (wav s) (mul s)))))

(deffilter fade 
  "Returns a wave that fades to 0 in amplitude over the course of len samples."
  [wav (len)]
  (stack
    p (pan 1 0 len)
    (gain wav p)))

;; EFFECT FILTERS

(deffilter simple-lowpass
  "Defines a simple lowpass filter, which works by averaging the current sample
   against an average of itself and the previous sample."
  [wav]
  (wave
   (let [cursamp (int (wav s))
	 prevsamp (int (hold))]
     (unchecked-add
       (unchecked-add (unchecked-divide cursamp 4) (unchecked-divide prevsamp 4))
       (unchecked-divide cursamp 2)))))

(deffilter feed-forward 
  "A simple feed-forward comb filter, representing a single echo. Takes either a fixed
   delay, or a fixed maximum delay and a delay wave.  g refers to the gain on
   the current line, while dg refers to the gain on the delayed line.

     |----------------(g)-----|
     |                        v
   [wav]-->[fdelay]--(dg)-->[add]-->"
  ([wav (del) g dg]
     (feed-forward wav del del g dg))
  ([wav (max-del) del g dg]
     (add 
      (gain wav g)
      (gain (buffer wav max-del del) dg))))

(deffilter feed-back 
  "A simple feed-back comb filter, representing an infinite number of echos.  Takes
   either a fixed delay, or a fixed maximum delay and a delay wave.  g refers to the 
   gain on the output line, while dg refers to the gain on the fed-back line.

             |-(g)--[out]--->
             |
   [wav]-->[res]----->[line]
             ^          |
             |---(dg)---|         "
  ([wav (del) g dg]
      (feed-back wav del del g dg))
   ([wav (max-del) del g dg]
     (rstack
      line (rdelay res max-del del)
      res (add wav (gain line dg))
      out (gain res g))))


(deffilter all-pass 
  "A simple all-pass filter using a combination of feed-forward and feed-back comb filters. 
   Takes a fixed delay, or a fixed maximum delay and a delay wave.

              |----(g)----------|
              |                 v
   [wav]-->[front]-->[line]-->[back]-->
              ^        |
              |--(-g)--|                 "
  ([wav (del) g]
     (all-pass wav del del g))
  ([wav (max-del) del g]
     (rstack
      front (add wav (gain line (negate g)))
      line (rdelay front max-del del)
      back (add line (gain front g)))))
