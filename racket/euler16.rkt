#|
  Euler #16 in Racket

  Problem 16
  """
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  
  What is the sum of the digits of the number 2^1000?
  """


  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)

(require "utils_hakank.rkt")

(define (euler16a)
  (list-sum (number->digits (expt 2 1000)))
  )

(define (run)
  (time-function euler16a)
  )

(run)


