#|
  Euler #9 in Racket

  Problem 9
  """
  A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
  a^2 + b^2 = c^2

  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)

(require (only-in "utils_hakank.rkt"
                  time-function
                  ))

;; cpu time: 67 real time: 67 gc time: 0
(define (euler9a)
  (for*/first ([a (range 1 500)]
               [b (range a 500)]
               [c (range b 500)]
               #:when (and (= (+ a b c) 1000) (= (- (+ (expt a 2) (expt b 2)) (expt c 2)) 0)))
    (* a b c)
    )
  )

(define (run)
  (time-function euler9a)
  )

(run)



