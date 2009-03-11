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

; define a guitar instrument
(deffilter guitar [(freq)]
  (if (= freq 0)
    (wave 0)
    (karplus-strong (white-noise freq) freq 0.98)))

; create the melody
(deftrack lead1 {:type :note :bpn 4}
  [ (e  f * g * d5 * *) (c   g4 ) (c   a    g   *  )  d      ]
  [ (e  f * g * d5 * *) (c   g4 ) (c   a    g   *  )  d      ]
  [ (a  a g a * b  * *) (c5  g4 ) (a * * c5 * * f *) (e  g4) ]
  [ (a  a g a * b  * *) (c5  e  )  c                  g4     ]
  [ (c5 c c c * c  * c) (c   .  )                            ])

; the same melody but down an octave
(def lead2 (transpose -12 lead1))


(with-bpm 200
  (time (render ["icarus.wav" (beats 72)]
    (mix
     (with-track [lead2] (guitar lead2))
     (gain (sine lead1) 0.1)))))
