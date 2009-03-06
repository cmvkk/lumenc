;; demo.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.demo
  (:refer-clojure)
  (:use lumenc.main)
  (:use lumenc.track)
  (:use lumenc.standards)
  (:use lumenc.dws))


(deftrack unowen
  [ d       a      e         a     ]
  [ f       (g a)  g         b     ]
  [ (d5 a4) (e5 f) (e (f e)) (d c) ]
  [ (a4 c5) (g4 a) f         h     ])

(deffilter pchord []
  (mix
          (karplus-strong (white-noise 220) 220 0.98)
   (shift (karplus-strong (white-noise 330) 330 0.98) (secs 0.05))
   (shift (karplus-strong (white-noise 440) 440 0.98) (secs 0.1))))

(deffilter schord []
  (mix
          (sine 440)
   (shift (sine 660) (secs 0.05))
   (shift (sine 880) (secs 0.1))))

(render ["demo-chord.wav" (secs 2)]
  (pchord))

(render ["demo-melody.wav" (secs 8)]
  (do-track #(karplus-strong (white-noise %) % 0.98) (transpose -12 unowen)))

