#|
  Euler #10 in Racket

  Problem 10
  """
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  
  Find the sum of all the primes below two million.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; Why doesn't math/number-theory have a primes function?
;;; (require math/number-theory)

(require (only-in "utils_hakank.rkt"
                 time-function primes list-sum
                 ))

;;; Not bad at all...
;;; cpu time: 75 real time: 75 gc time: 14
(define (euler10a)
  ;;; (primes-slow1 2000000) ; cpu time: 2204 real time: 2207 gc time: 23
  (list-sum (primes 2000000)) ; cpu time: 75 real time: 75 gc time: 14
  )


(define (run)
  (time-function euler10a)
  )

(run)



