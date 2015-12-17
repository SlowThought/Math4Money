#lang racket

(require rackunit)
(module+ test
  (printf "keno.rkt running tests."))

#| Describe a Keno pay table with a structure of form
   '(label 
      (spots (matches payoff)...)
      ...) |#

(provide/contract
 (keno-pt? (-> any/c boolean?)))

(define (keno-pt? x)
   (and (string? (car x)) ; First item is a string describing the particular pay table, ie "NY Quick Draw"
          (for/fold
              [(valid? #t)]
              [(y (cdr x))]
            (and valid? (valid-spot-rec? y))))) ; Rest should be spot payoff records

(define (valid-spot-rec? y)
  (and (exact-positive-integer? (car y)) ; spots played
       (for/fold
           [(valid? #t)]
           [(z (cdr y))]
         (and valid?
              (list? z)
              (= (length z) 2)
              (exact-nonnegative-integer? (car z)) ; spots matched
              (real? (cadr z))  ; payoff
              (positive? (cadr z))))))
            
(module+ test
  ; check spot rec test
  (check-true (valid-spot-rec? '(1 (1 2.))))
  (check-true (valid-spot-rec? '(2 (1 1.)(2 4.))))
  (check-false(valid-spot-rec? '(0 (1 2))))
  (check-false(valid-spot-rec? '(a (1 2))))
  (check-false(valid-spot-rec? '(2 (a 3/4))))
  (check-false(valid-spot-rec? '(2 (1 1.)(2))))
  ; check total test
  (check-true (keno-pt? '("Test table"
                          (1 (1 2)))))
  (check-true (keno-pt? '("Test table"
                          (3 (1 1)
                             (2 2)
                             (3 8)))))
  (check-false (keno-pt? '("Test table"
                           ('a (1 1))))))

;; Define some pay tables
(provide/contract
 (ny-quickdraw keno-pt?)
 (mi-club-keno keno-pt?))

; per betting slip dated 2/09
(define ny-quickdraw '("NY Quick Draw"
                       (10 (10 100000.)(9 5000.)(8 300.)
                           (7 45.)(6 10.)(5 2.)(0 5.))
                       (9  (9 30000.)(8 3000.)(7 125.)
                           (6 20.)(5 5.)(0 2.))
                       (8  (8 10000.)(7 550.)(6 75.)
                           (5 6.)(0 2.))
                       (7  (7 5000.)(6 100.)(5 20.)
                           (4 2.)(0 1.))
                       (6  (6 1000.)(5 55.)(4 6.)
                           (3 1.))
                       (5  (5 300.)(4 20.)(3 2.))
                       (4  (4 55.)(3 5.)(2 1.))
                       (3  (3 23.)(2 2.))
                       (2  (2 10.))
                       (1  (1 2.))))
; per betting slip dated 8/11
(define mi-club-keno '("MI Club Keno"
                       (10 (10 100000.)(9 5000.)(8 500.)
                           (7 50.)(6 10.)(5 2.)(0 5.))
                       (9  (9 25000.)(8 2000.)(7 100.)
                           (6 20.)(5 5.)(4 2.))
                       (8  (8 10000.)(7 300.)(6 50.)
                           (5 15.)(4 2.))
                       (7  (7 2000.)(6 100.)(5 11.)(4 5.)
                           (3 1.))
                       (6  (6 1100.)(5 57.)(4 7.)(3 1.))
                       (5  (5 410.)(4 18.)(3 2.))
                       (4  (4 72.)(3 5.)(2 1.))
                       (3  (3 27.)(2 2.))
                       (2  (2 11.))
                       (1  (1 2.))))
(module+ test
  (check-true (keno-pt? ny-quickdraw))
  (check-true (keno-pt? mi-club-keno)))
                          
;; Given a Keno pay table, generate a table of expected returns as a side effect,
;; returning nothing
(provide/contract (generate-return-table (-> keno-pt? void?)))
(require math/number-theory)

(define (generate-return-table keno-pt)
  (printf "~a~n" (car keno-pt))
  (printf (columns))
  (for [(bet-record (cdr keno-pt))]; '((spot (match payoff) ...)...)
    (let [(spot (car bet-record))
          (pay-records (cdr bet-record))]
      (let-values ([(total-wins expected-return)
                    (for/fold 
                        [(total-wins 0)
                         (expected-return 0.)]
                        [(match-record (cdr bet-record))]
                      (let*[(match(car match-record))
                            (payoff(cadr match-record))
                            (wins (* (binomial spot match)
                                     (binomial (- 80 spot)(- 20 match))))]
                        (values (+ total-wins wins)
                                (+ expected-return (* payoff (/ wins (total-games)))))))])
        (printf "   ~a        $~a          ~a~n"
                spot
                (round-2-digits expected-return)
                (round-2-digits (/ (total-games) total-wins))))))
  (printf (columns)))
;; Helpers

; Thunk lets me use before declaring -- like forward declare in C?
(define (columns) "-Spots--Expected Return--Odds of Winning-~n")

; Returns to nearest penny
(define (round-2-digits x)
    (/ (round (* x 100.)) 100.))

; Possible number of keno games
(define (total-games)
  (binomial 80 20))
