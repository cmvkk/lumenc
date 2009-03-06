;; lumenc/dws.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.dws
  (:refer-clojure)
  (:use lumenc.main)
  (:use lumenc.standards))


;; ALGOS

(deffilter karplus-strong 
  "A simple Karplus-Strong implementation, generating
   a guitar string.  freq and g can be waves, allowing
   for changes in frequency or string tension over time.

                         |---------->
                         |
   [pick]--->[res]--(g)--|-->[line]
               ^               |
               |--[decay]<-----|    "
  ([seed freq g]
     (karplus-strong seed (secs 0.1) freq g))
  ([seed (len) freq g]
     (let [first-per (/ *rate* (freq 0))]
       (rstack
	pick  (cap seed first-per)
	decay (simple-lowpass line)
	line  (rdelay res len (period freq))
	res   (gain (add pick decay) g)))))
