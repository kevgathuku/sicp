;; Helper functions
(defn square
  [x]
  (* x x))

(defn abs
  [x]
  (cond
    (< x 0) (- x)
    :else x))

(defn average
  [x y]
  (/ (+ x y) 2))

;; Sum of integers from a -> b
(defn sum-int
  [a b]
  (if (> a b)
    0
    (+ a
       (sum-int (inc a) b))))

;; Sum of the squares of integers from a -> b
;; Very similar pattern to sum-int function
(defn sum-sq
  [a b]
  (if (> a b)
    0
    (+ (square a)
       (sum-sq (inc a) b))))

;; General pattern
; (defn <name>
;   [a b]
;   (if (> a b)
;     0
;     (+ (<term> a)
;     (<name> (<next> a) b)))

;; where <name> <term> and <next> are functions

;; Sum Higher Order Function
(defn sum
  [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum
         term
         (next a)
         next b))))
         
(defn sum-int-hof
  [a b]
  (sum identity a inc b))


(defn sum-sq-hof
  [a b]
  (sum square a inc b))


;; Heron's square root method 
;; Find a fixed point (sqrt) by calling a function repeatedly
;; avg of y & (x / y)
(defn sqrt
  [x]
  (fixed-point 
    (fn [y] (average (/ x y) y))
    1))

;; let -> define local variables
;; letfn -> define local functions supporting recursion
(defn fixed-point
  [f start]
  (let [tolerance 0.00001]
   (letfn [(close-enuf? [u v]
                        (< (abs (- u v)
                             tolerance)))
           (iter [old new]
                 (if (close-enuf? old new)
                   new
                   (iter new (f new))))]
    iter start (f start))))
  
;; Simplification of above sqrt method
;; average-damp generates the fn passed to fixed-point
(defn sqrt-damp
  [x]
  (fixed-point
    average-damp (fn [y] (/ x y))
    1))

;; avg-damp -> returns fn that computes average of 2 oscillatting values
;; i.e. x and the applying the passed in fn to x. The fn can vary
;; takes in a fn and returns a fn
(defn average-damp
  [f]
  (fn [x] (average (f x) x)))


;; tests
(defn sum-int-checker
  []
  (assert (= 6 (sum-int 0 3)))
  (assert (= 6 (sum-int-hof 0 3))))
