#|
  Euler #12 in Racket

  Problem 12
  """
  The sequence of triangle numbers is generated by adding the natural numbers. 
  So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. 
  The first ten terms would be:

  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

  Let us list the factors of the first seven triangle numbers:

       1: 1
       3: 1,3
       6: 1,2,3,6
      10: 1,2,5,10
      15: 1,3,5,15
      21: 1,3,7,21
      28: 1,2,4,7,14,28

  We can see that the 7th triangle number, 28, is the first triangle number 
  to have over five divisors.

  Which is the first triangle number to have over five-hundred divisors?")
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in math/number-theory
                  divisors triangle-number
                  ))

(require (only-in "utils_hakank.rkt"
                  time-function num-divisors
                  ))

;;; Not too bad
;;; cpu time: 440 real time: 441 gc time: 26
(define (euler12a)
  (for/first ([n (in-naturals)]
              #:when (> (length (divisors (triangle-number n))) 500))
    (triangle-number n)
  ))

;;; A little faster:
;;; cpu time: 278 real time: 278 gc time: 24
(define (euler12b)
  (let ([tnum 0])
    (for/first ([n (in-naturals 1)]
                #:do [(set! tnum (+ tnum n))]                
                #:when (> (length (divisors tnum)) 500))
      tnum)
    )
  )

;; Triangle number 
(define (tri n)
  (/ (* n (add1 n)) 2))


;;; A little faster than euler12b
;;; cpu time: 246 real time: 259 gc time: 13
(define (euler12c)
  (for/first ([n (in-naturals 1)]
              #:when (> (num-divisors (tri n)) 500))
    (tri n))
  )

(define (run)
  ;;; (time-function euler12a)
  ;;; (time-function euler12b)
  (time-function euler12c)  
  )

(run)



