;; Lec 1a

(ns intro.core
  (:gen-class))

(defn square
  [x]
  (* x x))

(defn average
  [x y]
  (/ (+ x y) 2))

(defn abs
  [x]
  (cond
    (< x 0) (- x)
    :else x))

(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn good-enough?
  [guess x]
  (< (abs (- (square guess) x)) 0.001))


(defn sqrt-guesser
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-guesser (improve guess x) x)))


;; Start with a guess of 1
(defn sqrt
  [x]
  (sqrt-guesser 1.0 x))

(defn -main []
  (println (sqrt 36)))
