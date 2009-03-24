

(is (= (take 10 (impulse))  '(1 0 0 0 0 0 0 0 0 0)))
(is (= (take 10 (repeat 1)) '(1 1 1 1 1 1 1 1 1 1)))

(is (= (take 10 (cap        (repeat 1)     5)) '(1 1 1 1 1 0 0 0 0 0)))
(is (= (take 10 (shift      (impulse)      5)) '(0 0 0 0 0 1 0 0 0 0)))
(is (= (take 10 (shift (cap (repeat 1) 5) -2)) '(1 1 1 0 0 0 0 0 0 0)))

(is (= (take 10 (buffer (impulse) 5      5)) '(0 0 0 0 0    1    0 0 0 0)))
(is (= (take 10 (buffer (impulse) 10     5)) '(0 0 0 0 0    1    0 0 0 0)))
(is (= (take 10 (buffer (impulse) 5  41/10)) '(0 0 0 0 9/10 1/10 0 0 0 0)))

(is (= (take 20 (pan 10 1 10)) '(10 9 8 7 6 5 4 3 2 1 1 1 1 1 1 1 1 1 1 1)))
(is (= (take 20 (pan 1 10 10)) '(1 2 3 4 5 6 7 8 9 10 10 10 10 10 10 10 10 10 10 10)))

(is (= (take 10 (exp-drop 2))                  '(1 1/2 1/4 1/8 1/16 1/32 1/64 1/128 1/256 1/512)))
(is (= (take 10 (pluck-envelope (repeat 1) 2)) '(1 1/2 1/4 1/8 1/16 1/32 1/64 1/128 1/256 1/512)))

(is (= (parallel (repeat 1)) '(1 1 1 1 1 1 1 1 1 1 1 1 1)))

(is (= (take 10 (negate (repeat 1))) '(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1)))

(is (= (take 10 (invert-gain (repeat 1/9))) '(8/9 8/9 8/9 8/9 8/9 8/9 8/9 8/9 8/9 8/9)))

(is (= (take 3 (period 441)) '(100 100 100)))

(let [wn (take 50 (white-noise))]
  (is (not= (wn 10) (wn 11)))
  (is (not= (wn 20) (wn 21)))
  (is (not= (wn 48) (wn 49))))

(is (= (take 5 (gain (repeat 1) 1/2)) '(1/2 1/2 1/2 1/2 1/2)))

(is (= (take 10 (fade (repeat 10) 5)) '(10 8 6 4 2 0 0 0 0 0)))



