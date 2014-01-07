#lang racket
(require rackunit)

(module+ test
  (printf "probability.rkt running tests.~n"))

;; Combinatorial function - number of ways to choose k items from a group of n items
(provide/contract 
 (choose (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)))

(define (choose n k)
  (let loop [(i 0)
             ; First product will hold results of n x (n-1) x ... (n-k+1)
             (p1 1)
             ; Second product will contain k x (k-1) x ... x 2 x 1
             (p2 1)]
    (if (< i k)
        (loop (add1 i)
              (* p1 (- n i))
              (* p2 (add1 i)))
        ; Return the answer
        (/ p1 p2))))

(module+ test
  (check-eq? (choose 3 0) 1)
  (check-eq? (choose 3 1) 3)
  (check-eq? (choose 3 2) 3)
  (check-eq? (choose 3 3) 1))