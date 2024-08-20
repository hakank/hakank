#|
  Euler #28 in Racket

  Problem 28
  """
  Starting with the number 1 and moving to the right in a clockwise 
  direction a 5 by 5 spiral is formed as follows:
  
     21 22 23 24 25
     20  7  8  9 10
     19  6  1  2 11
     18  5  4  3 12
     17 16 15 14 13

  It can be verified that the sum of the numbers on the diagonals is 101.
  
  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function list-sum
                  ))

;;; cpu time: 0 real time: 0 gc time: 0
(define (euler28a)
  (let ([s 1]
        [n 3])
    (for/last ([t (in-naturals 1)]
               #:break (> n 1001)
               #:do [(set! s (+ s (* 4 (expt n 2)) (- (* 6 n)) 6 ))
                     (set! n (+ n 2))])
      s)
    )
  )

;;; Neater.
;;; cpu time: 0 real time: 0 gc time: 0
(define (euler28b)
  (add1 (list-sum (for/list ([n (range 3 1002 2)])
    (+ (* 4 (expt n 2)) (- (* 6 n)) 6)
    )))
  )

(define (run)
  ;;; (time-function euler28a)
  (time-function euler28b)
  )

(run)



