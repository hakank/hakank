#|
  Euler #3 in Racket

  Problem 3
  """
  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143 ?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require math/number-theory)
;;; This is not faster
;; (require (only-in math/number-theory
;;                   factorize
;;                   ))
(require "utils_hakank.rkt")

;;; cpu time: 26 real time: 26 gc time: 9
(define (euler3a)
  (apply max (map car (factorize 600851475143)))
  )

;;; Brute force:
;;; cpu time: 32 real time: 32 gc time: 1
(define (euler3b)
  (let* ([num 600851475143]
         [limit (sqrt num)])
    (for/last ([i (range 2 limit)]
               #:when (and (= (modulo num i) 0) (prime? i)))
      i)
    )
  )

(define (run)
  (time-function euler3a)
  ;;; (time-function euler3b)
  )

(run)

