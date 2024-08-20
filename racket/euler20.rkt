#|
  Euler #20 in Racket

  Problem 
  """
  n! means n (n 1) ... 3 2 1

  Find the sum of the digits in the number 100!")
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in math/number-theory
                  factorial
                  ))
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function list-sum number->digits
                  ))

;;; cpu time: 0 real time: 0 gc time: 0
(define (euler20a)
  (list-sum (number->digits (factorial 100)))
  )

(define (run)
  (time-function euler20a)
  )

(run)




