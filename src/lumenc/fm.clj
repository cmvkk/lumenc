;; lumenc/fm.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.fm
  (:refer-clojure)
  (:use lumenc.main)
  (:use lumenc.standards))


(deffilter stretch 
  "Changes the bounds on a wav from *max-amp* to
   from top to bottom."
  [wav (top) (bottom)]
  (let [mid (/ (+ top bottom) 2)
	mult (/ (/ (- top bottom) 2) *max-amp*)]
  (wave
   (+ mid (* mult (wav s))))))



(deffilter psine 
  "A sine wave, with changeable frequency and phase."
  [freq phase]
  (let [lastrad (atom 0)
	rad (/ (* 2 Math/PI) *rate*)]
    (wave
     (let [radjump (* rad (freq s))
	   newrad (+ @lastrad radjump)]
       (swap! lastrad (fn [x] newrad))
       (* *max-amp* (Math/sin (+ newrad (phase s))))))))

(deffilter sine 
  "A sine wave with changeable frequency."
  [freq]
  (psine freq 0))



(deffilter ppulse 
  "A pulse wave with changeable frequency, duty, and phase."
  [freq duty phase]
  (let [lastpos (atom 0)
	step (/ 1 *rate*)]
    (wave
     (let [sjump (* step (freq s))
	   newpos (+ @lastpos sjump)]
       (swap! lastpos (fn [x] newpos))
       (if (< (mod (+ (phase s) newpos) 1) (duty s))
	 *max-amp* (- *max-amp*))))))

(deffilter pulse 
  "A pulse wave with changeable frequency and duty."
  [freq duty]
  (ppulse freq duty 0))

(deffilter psquare 
  "A square wave with changeable frequency and phase."
  [freq phase]
  (ppulse freq 0.5 phase))

(deffilter square 
  "A square wave with changeable frequency."
  [freq]
  (ppulse freq 0.5 0))



(deffilter ptriangle 
  "A triangle wave with changeable frequency, duty, and phase."
  [freq duty phase]
  (let [lastpos (atom 0)
	step (/ 1 *rate*)
	range (* 2 *max-amp*)]
    (wave
     (let [sjump (* step (freq s))
	   newpos (+ @lastpos sjump)
	   cpos (mod (+ newpos (phase s)) 1)
	   cd (duty s)]
       (swap! lastpos (fn [x] newpos))
       (if (< cpos cd)
	 (- (/ (*    cpos        range)       cd)  *max-amp*)
	 (+ (/ (* (- cpos cd) (- range)) (- 1 cd)) *max-amp*))))))

(deffilter triangle 
  "A triangle wave with changeable frequency and duty."
  [freq duty]
  (ptriangle freq duty 0))

(deffilter psawtooth 
  "A sawtooth wave with changeable frequency and phase."
  [freq phase]
  (ptriangle freq 1 phase))

(deffilter sawtooth 
  "A sawtooth wave with changeable frequency."
  [freq]
  (ptriangle freq 1 0))
