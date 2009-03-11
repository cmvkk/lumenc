;; icarus.clj
;; copyright 2005 William D. Lipe (cmvkk)

;; The melody from World 3 of Nintendo's Kid Icarus.

(ns lumenc.icarus
  (:refer-clojure)
  (:use lumenc.main)
  (:use lumenc.standards)
  (:use lumenc.track)
  (:use lumenc.fm)
  (:use lumenc.dws))



(deffilter guitar [(freq)]
  (karplus-strong (white-noise freq) freq 0.98))

(deftrack lead1 {:type :note :bpn 4}
  [ (e  f * g * d5 * *) (c   g4 ) (c   a    g   *  )  d     ]
  [ (e  f * g * d5 * *) (c   g4 ) (c   a    g   *  )  d     ]
  [ (a  a g a * b  * *) (c5  g4 ) (a * * c5 * * f *) (e g4) ]
  [ (a  a g a * b  * *) (c5  e  )  c                  g4    ]
  [ (c5 c c c * c  * c) c                                   ])

(def lead2 (transpose -12 lead1))

(with-bpm 240
  (time (render ["icarus.wav" (beats 72)]
    (mix
     (with-track [lead1] (guitar lead1))
     (gain (sine lead2) 0.3)))))
