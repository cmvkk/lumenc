;; lumenc/fm.clj
;; copyright 2009 William D. Lipe (cmvkk)

(ns lumenc.fm
  (:refer-clojure)
  (:use lumenc.main)
  (:use lumenc.standards))


(deffilter stretch
  "Changes the bounds on a wav from *max-amp* to 
   from top to bottom."
  [wav top bottom]
  (let [mid (/ (+ top bottom) 2)
	mult (/ (/ (- top bottom) 2) *max-amp*)]
    (wave [wav]
      (give (+ mid (* mult wav))))))

(deffilter psine 
  "A sine wave, with changeable frequency and phase."
  [freq phase]
  (let [rad (/ (* 2 Math/PI) *rate*)]
    (wave [freq phase :and lastrad 0]
     (let [radjump (* rad freq)
	   newrad (+ lastrad radjump)]
       (give (* *max-amp* (Math/sin (+ newrad phase)))
	     newrad)))))

(deffilter sine 
  "A sine wave with changeable frequency."
  [freq]
  (psine freq 0))



(deffilter ppulse 
  "A pulse wave with changeable frequency, duty, and phase."
  [freq duty phase]
  (let [step (/ 1 *rate*)]
    (wave [freq duty phase :and lastpos 0]
     (let [sjump (* step freq)
	   newpos (+ lastpos sjump)]
       (give (if (< (mod (+ phase newpos) 1) duty)
	       *max-amp* (- *max-amp*))
	     newpos)))))

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
  (let [step (/ 1 *rate*)
	range (* 2 *max-amp*)]
    (wave [freq duty phase :and lastpos 0]
     (let [sjump (* step freq)
	   newpos (+ lastpos sjump)
	   cpos (mod (+ newpos phase) 1)]
       (give (if (< cpos duty)
	       (- (/ (*    cpos          range)       duty)  *max-amp*)
	       (+ (/ (* (- cpos duty) (- range)) (- 1 duty)) *max-amp*))
	     newpos)))))



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
