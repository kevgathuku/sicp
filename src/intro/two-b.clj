;; Compound Data
;; Building blocks

(defn average
  [x y]
  (/ (+ x y) 2))

(defn square
  [x]
  (* x x))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn cons [x y]
  (list x y))

(defn car [x]
  (first x))

(defn cdr [x]
  (second x))

;; cons, car and cdr alternative representation
(defn cons-lisp [a b]
  (defn [pick] 
    (cond
      ((= pick 1) a)
      ((= pick 2) b))))

(defn car-lisp [x]
  (x 1))

(defn cdr-lisp [x]
  (x 2))

;; Rational numbers
;; Abstraction layer -> Constructor + selectors (DATA ABSTRACTION)
;; Helps abstracts "pairs"(representation) from rational numbers (use)
;; Reducing things to the lowest terms / building blocks

;; use GCD to reduce numbers to the lowest terms
(defn make-rat [n d]
  (let [g (gcd n d)]
    (cons 
      (/ n g)
      (/ d g))))
  
(defn numer [x]
  (car x))

(defn denom [x]
  (cdr x))

(defn +rat [x y]
  (make-rat 
    (+ (* (numer x) (denom y))
       (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(defn *rat [x y]
  (make-rat 
    (* (numer x) (numer y))
    (* (denom x) (denom y))))


;; Testing
(def a (make-rat 1 2))

(def b (make-rat 1 4))

(println (+rat a b))

;; Representing vectors / points on a plane

(defn make-vector [x y]
  (cons x y))

(defn xcor [p] (car p))

(defn ycor [p] (cdr p))

;; Representing line segments
;; pairs can contain compound data e.g. vectors, not just numbers

(defn make-segment [p q]
  (cons p q))

(defn seg-start [s] (car s))

(defn seg-end [s] (cdr s))

(defn midpoint [s]
  (let [a (seg-start s)
        b (seg-end s)]
    (make-vector
      (average (xcor a) (xcor b))
      (average (ycor a) (ycor b)))))


(defn segment-length [s]
  (let [dx (- (xcor (seg-end s))
              (xcor (seg-start s)))
        dy (- (ycor (seg-end s))
              (ycor (seg-start s)))]
    (Math/sqrt (+ (square dx)
                  (square dy)))))

