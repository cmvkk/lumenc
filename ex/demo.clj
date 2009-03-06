;; demo.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.demo
  (:refer-clojure)
  (:use lumenc.main)
  (:use lumenc.track)
  (:use lumenc.standards)
  (:use lumenc.dws))

; an example of defining one track
(deftrack unowen {:type :note}
  [ d       a      e         a     ]
  [ f       (g a)  g         b     ]
  [ (d5 a4) (e5 f) (e (f e)) (d c) ]
  [ (a4 c5) (g4 a) f         h     ])


; an example of defining multiple tracks
(deftrack
  dm1 dm2 dm3 {:type :note :bpn 2 :oct 3}

  dm1 [ (a c4)  e        (d g)  e        ]
  dm2 [  r     (r c h h)  h    (h c h h) ]
  dm3 [  r     (r   e  )  h    (h   e  ) ]

  dm1 [ (d a)   e        b     c5        ]
  dm2 [  h     (h e h h) b2    a         ]
  dm3 [  h     (h   c  ) r     r         ])


(deffilter guitar [(freq)]
  (karplus-strong (white-noise freq) freq 1))


(deffilter deep-mountain []
  (mix
         (do-track guitar dm1)
   (gain (do-track guitar dm2) 0.25)
   (gain (do-track guitar dm3) 0.25)))


(render ["unowen.wav" (beats 16)]
  (do-track guitar (transpose -12 unowen)))

(render ["deep-mountain.wav" (beats 16)]
  (deep-mountain))
