;; lumenc/standards.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.standards
  (:refer-clojure)
  (:use lumenc.main))

;; UTILITY FILTERS

(deffilter impulse 
  "Impulse filter.  Returns 1 for the first sample, 0 otherwise."
  []
  (lazy-seq (cons 1 (repeat 0))))

(deffilter one 
  "A filter that returns only 1.  Mostly for testing purposes."
  []
  (repeat 1))


(deffilter pan
  "Returns a wave that goes from 'start'
  to 'end' in 'len' samples."
  [start end len]
  (wave [:and s 0]
    (give (+ (* end   (/ s         len))
             (* start (/ (- len s) len)))
	  (inc s))))

(deffilter exp-drop
  "Equals 1/(c^s) where c is coeff and s is samples.  The higher coeff, the
   sharper the drop. The values here are pretty small.  A coeff of 1.0002 will
   drop off in half a second."
  [coeff]
  (wave [:and s 0]
    (give (/ 1 (Math/pow coeff s)) 
	  (inc s))))

;(deffilter parallel
;  "Causes the given wave's generation to be spun off
;   into its own thread, making it concurrent.  Up to
;   tsamps samples will be generated, after which the 
;   wave will end."
;  [wav]
;  (seque 500 wav))


(deffilter parallel
  [wav len]
  (let [lbq (new java.util.concurrent.LinkedBlockingQueue)
	a (agent wav)
	len2 (int (+ len 3))
	foo (fn [s]
	      (loop [s s i 0]
		(.put lbq (first s))
		(if (and (next s) (< i len2))
		  (recur (next s) (inc i))
		  nil)))]
    (send-off a foo)
    (wave []
      (give (.take lbq)))))



;(deffilter parallel
;  [wav]
;  wav)

(deffilter negate
  "Negates a wave, flips it upsidown over 0."
  [wav]
  (wave [wav]
    (give (- wav))))

(deffilter invert-gain
  "Inverts an amplitude, so that louder is quieter."
  [wav]
  (wave [wav]
   (give (- 1 wav))))


(deffilter period 
  "Converts a wave returning frequencies to one returning
   period lengths."
  [freq]
  (wave [freq]
   (give (/ *rate* freq))))

(deffilter cap 
  "'Caps' a wave after a certain time, returning only silence
   after that point."
  [wav len]
  (wave [wav :and s 0]
   (if (> s len)
     (give 0 s)
     (give wav (inc s)))))

;; DELAYS

(deffilter shift
  "'Shifts' a wave forward or backward in time.  A positive len
   will delay the wave for len samples, while a negative len
   will crop the front of the wave for len samples.  This is done
   in a cache-less manner, so it's safe to use with very large
   values of len without worrying about memory.  However, len
   must be a whole number."
  [wav len]
  (cond
    (zero? len) wav
    (pos? len) (concat (repeat len 0) wav)
    (neg? len) (drop (- len) wav)))

(defn interpolate [low high frac]
  (+ (* frac low) (* (- 1 frac) high)))

(deffilter buffer
  "Delays a wave for dlen samples.  Size is the 'maximum delay' the buffer
   will be capable of.  dlen can be a track or wave, allowing for shifts in delay
   length over time, as long as they don't exceed size.  dlen can also be fractional
   (the return value will be calculated with linear interpolation), and size can be fractional
   but will be rounded up."
  [wav size dlen]
  (let [size (int (Math/ceil size))]
    (cons 0 (wave [wav dlen :and 
                   hold (into [] (repeat size 0))
		   s 0]
	      (let [ns (- s (dec dlen))
		    ntop (mod (int (Math/ceil ns)) size)
		    nbot (mod (int (Math/floor ns)) size)
		    res (interpolate (get hold nbot) (get hold ntop) (mod ns 1))]
;		(println "ntop" ntop "nbot" nbot "ns" ns "s" (mod (dec s) size))
		(give res (assoc hold (mod s size) wav) (inc s))))))) 



(deffilter white-noise
  "Returns random data."
  []
  (let [random (new java.util.Random)]
    (wave []
      (give (unchecked-divide (.nextInt random) 64)))))

;; VOLUME FILTERS

(deffilter gain 
  "Returns a wave multiplied by a certain gain.  The gain can be a wave,
   allowing for gain changes over time."
  [wav mul]
  (wave [wav mul]
    (give (* wav mul))))

(deffilter fade 
  "Returns a wave that fades to 0 in amplitude over the course of len samples."
  [wav len]
  (stack
    p (pan 1 0 len)
    (gain wav p)))

(deffilter pluck-envelope
  "A volume envelope with an exponential drop, like a plucked string would have."
  [wav coeff]
  (gain wav (exp-drop coeff)))


;; EFFECT FILTERS

(deffilter simple-lowpass
  [wav]
  (wave [wav :and last 0]
    (let [res (/ (+ (/ (+ last wav) 2)
		    wav) 2)]
      (give res res))))

(deffilter feed-forward 
  "A simple feed-forward comb filter, representing a single echo. Takes either a fixed
   delay, or a fixed maximum delay and a delay wave.  g refers to the gain on
   the current line, while dg refers to the gain on the delayed line.

     |----------------(g)-----|
     |                        v
   [wav]-->[fdelay]--(dg)-->[add]-->"
  ([wav del g dg]
     (feed-forward wav del del g dg))
  ([wav max-del del g dg]
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
  ([wav del g dg]
      (feed-back wav del del g dg))
   ([wav max-del del g dg]
     (rstack
      line (buffer res max-del del)
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
  ([wav del g]
     (all-pass wav del del g))
  ([wav max-del del g]
     (rstack
      front (add wav (gain line (negate g)))
      line (buffer front max-del del)
      back (add line (gain front g)))))
