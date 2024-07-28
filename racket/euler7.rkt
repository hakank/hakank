#|
  Euler #7 in Racket

  Problem 7
  """
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
  that the 6^(th) prime is 13.

  What is the 10001^(st) prime number?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require math/number-theory)

(require "utils_hakank.rkt")

;; Note: The first call takes a little longer, assumingly to load the stuff in the math package.
;; So time the functions separately

;; cpu time: 29 real time: 29 gc time: 4
(define (euler7a)
  (nth-prime 10000) ; Racket is 0-based
  )

;; cpu time: 28 real time: 28 gc time: 4
(define (euler7b)
  (let ([p 2])
    (for ([n (range 1 10001)])
          (set! p (next-prime p))
          )
    p)
  )

(define (run)
  (time-function euler7a)
  ;;; (time-function euler7b)
  )

(run)

