;; Lec 1b

(ns intro.one-b
  (:gen-class))

(defn sq
  [x]
  (* x x))

(defn sum-of-squares
  [a b]
  (+ (sq a) (sq b)))

;; Substitution rule
(sum-of-squares 3 4)

(+ (sq 3) (sq 4))

(+ (sq 3) (* 4 4))

(+ (sq 3) 16)

(+ (* 3 3) 16)

(+ 9 16)

25

;; Evaluating conditionals

(defn sum-a
  [x y]
  (if (= x 0)
    y
    (sum-a (dec x) (inc y))))

;; Recursive definition, but iterative algorithm
;; Has concept of accumulator
;; time => O(x)
;; space => O(1)
(sum-a 3 4)

(sum-a 2 5)

(sum-a 1 6)

(sum-a 0 7)

7

;; Variation of sum-a
(defn sum-b
  [x y]
  (if (= x 0)
    y
    (inc (sum-b (dec x) y))))
    

;; Builds up numbers to increment later
;; Linear Recursion (proportionate to arg x in both time and space)
;; time => O(x)
;; space => O(x)

(sum-b 3 4)

(inc (sum-b 2 4))

(inc (inc (sum-b 1 4)))

(inc (inc (inc (sum-b 0 4))))

(inc (inc (inc 4)))

(inc (inc 5))

(inc 6)

7


;; Fibonacci
;; time = O(fib(n))
;; space = O(n)
(defn fib
  [n]
  (if (< n 2)
    n
    (+ (fib (dec n))
       (fib (- n 2)))))

(fib 10)


;; Towers of hanoi
;; Iterative algorithm, defined recursively
(defn towers-of-hanoi
  [n from to via]
  (if 
    (= n 1) (println (format "Move from %s to %s" from to))
    (do
      (towers-of-hanoi (dec n) from via to)
      (println (format "Move from %s to %s" from to))
      (recur (dec n) via to from))))

(towers-of-hanoi 4 1 2 3)

