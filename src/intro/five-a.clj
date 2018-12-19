;; Assignment and local state

;; Factorial function written in the pure form
;; Substitution model works for this function
(defn fact [n]
  (letfn [(iter [acc count]
            (if (> count n) acc
                (iter (* acc count) (+ count 1))))]
    (iter 1 1))) 

;; Factorial function using mutable state
;; Substitution model does not work here
;; do executes each of its arguments in sequence and returns the result of the last one.
(defn fact-mutable [n]
  (let [count (atom 1) acc (atom 1)]
    (letfn [(loop []
              (if (> @count n) @acc
                  (do 
                    (swap! acc (fn [tmp-acc] (* tmp-acc @count)))
                    (swap! count inc)
                    (loop))))]
      (loop))))

