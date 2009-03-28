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
  [ d        a+    e         a     ]
  [ f       (g a)  g         b     ]
  [ (d  a)  (e+ f) (e (f e)) (d c) ]
  [ (a  c)  (g  a) f         *     ])


; an example of defining multiple tracks
(deftrack
  dm1 dm2 dm3 {:type [:note :rest-zero] :bpn 2 :oct 4}

  dm1 [ (a c)  e        (d g)  e        ]
  dm2 [  .     (. c * *)  *    (* c * *) ]
  dm3 [  .     (.   e  )  *    (*   e  ) ]

  dm1 [ (d a+)  e        b+    c         ]
  dm2 [  *     (* e * *) b     a         ]
  dm3 [  *     (*   c  ) .     .         ])


(deffilter guitar [freq]
  (if (= freq 0)
    (repeat 0)
    (karplus-strong (white-noise) freq 0.98)))


(deffilter deep-mountain []
  (mix
         (with-track [dm1] (guitar dm1))
   (gain (with-track [dm2] (guitar dm2)) 0.25)
   (gain (with-track [dm3] (guitar dm3)) 0.25)))


(render ["unowen.wav" (beats 16)]
  (with-track [unowen] (guitar unowen)))

(render ["deep-mountain.wav" (beats 16)]
  (deep-mountain))
