;; demo.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.demo
  (:refer-clojure)
  (:use lumenc.main)
  (:use lumenc.track)
  (:use lumenc.standards)
  (:use lumenc.dws))

; an example of defining one track
(deftrack unowen {:type [:note]}
  [ d       a      e         a     ]
  [ f       (g a)  g         b     ]
  [ (d5 a4) (e5 f) (e (f e)) (d c) ]
  [ (a4 c5) (g4 a) f         *     ])


; an example of defining multiple tracks
(deftrack
  dm1 dm2 dm3 {:type [:note] :bpn 2 :oct 3}

  dm1 [ (a c4)  e        (d g)  e        ]
  dm2 [  .     (. c * *)  *    (* c * *) ]
  dm3 [  .     (.   e  )  *    (*   e  ) ]

  dm1 [ (d a)   e        b     c5        ]
  dm2 [  *     (* e * *) b2    a         ]
  dm3 [  *     (*   c  ) .     .         ])


(deffilter guitar [(freq)]
  (if (= freq 0)
    (constantly 0)
    (karplus-strong (white-noise freq) freq 0.98)))


(deffilter deep-mountain []
  (mix
         (do-track guitar dm1)
   (gain (do-track guitar dm2) 0.25)
   (gain (do-track guitar dm3) 0.25)))


(time (render ["unowen.wav" (beats 16)]
  (do-track guitar (transpose -12 unowen))))

(time (render ["deep-mountain.wav" (beats 16)]
  (deep-mountain)))
