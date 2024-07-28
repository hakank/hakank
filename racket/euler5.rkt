#|
  Euler #5 in Racket

  Problem 5
  """
  2520 is the smallest number that can be divided by each of the numbers 
  from 1 to 10 without any remainder.

  What is the smallest number that is evenly divisible by all of the numbers 
  from 1 to 20?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require math/number-theory)

(require "utils_hakank.rkt")

;; cpu time: 0 real time: 0 gc time: 0
(define (euler5a)
  (apply lcm (range 2 20))
  )

;; Brute force
;;; cpu time: 8134 real time: 8134 gc time: 12
(define (euler5b)
  (for*/first ([n (in-naturals 2)]
               #:when (for/and ([i (range 2 21)]) (= (modulo n i) 0)  ))
    
    n)
  )

(define (run)
  (time-function euler5a)
  ;;; (time-function euler5b) ; Too slow
  )

(run)

