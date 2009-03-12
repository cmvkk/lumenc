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

; create the melody
(deftrack 
  lead1 {:type [:arpeggio :note] :alen 1/4 :bpn 4}
  vol1  {:type [:arpeggio :option] :alen 1/4 :bpn 4 
	 :opts {0 0 1 1/10 2 2/10 3 3/10 4 4/10 5 5/10 6 6/10 7 7/10 8 8/10 9 9/10 10 1}}
  bass1 {:type [:note] :bpn 4 :oct 2}

  lead1 [ (e  f * g * [d5 g4] * *) ([c5 f4] .   ) (c   a    g   [e4 a3]  ) ([d4 g3] .     )]
   vol1 [ 10                       10             (9   10   9   [7  5 ]  )  [6  4 ]        ]
  bass1 [ (c3   .   e2        .  ) (d  .    g  .) (f   .    e   .        ) (a#   .  g  .  )]

  lead1 [ (e4 f * g * [d5 g4] * *) ([c5 f4] .   ) (c   a    g   [e4 a3]  ) ([d4 g3] .     )]
   vol1 [ 10                       10             (9   10   9   [7  5 ]  )  [6  4 ]        ]
  bass1 [ (c3   .   e2        .  ) (d .     g  .) (f   .    e   .        ) (a#   .  g  .  )]

  lead1 [ (a4 a g a * b  * *) (c5  g4  ) ([a d] * * [c5 f4] * * [f5 a#4] *) ([e5 a4] g4   )]
   vol1 [ 10                  10         (8     * *  9      * * 10       *) (9       7    )]
  bass1 [ (f  f e f * g  * *) (a . e  .) (f       .         f   .         ) (c3  .   c .  )]

  lead1 [ (a  a g a * b  * *) (c5  e   )  c                  g4               ]
   vol1 [ 10                  10          10                 10               ]
  bass1 [ (f2 f e f * g  * *) (a . e3 .) (c d c d  c d c d) (g2 a g a g a g a)]

  lead1 [ (c5 c c c * c  * c) (c   .   )                                      ]
   vol1 [ 10                  10                                              ]
  bass1 [ (c3 c c c * c  * c) (c . . . )                                      ])


; define an instrument
(deffilter lead-instr [freq]
  (gain (square freq) (exp-drop 1.00005)))

(with-bpm 220
  (time (render ["icarus.wav" (beats 48)]
    (mix (gain (gain (with-track [lead1] (lead-instr lead1)) vol1) 0.5)
	 (triangle bass1 0.5)))))
