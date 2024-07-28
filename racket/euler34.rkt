#|
  Euler #34 in Racket

  """
  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  
  Find the sum of all numbers which are equal to the sum of the 
  factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require math/number-theory)
;;; (require racket/trace)

(require "utils_hakank.rkt")

;;; cpu time: 37 real time: 37 gc time: 6
(define (euler34a)
  (for/sum ([n (range 10 100001)]
            #:when (= n (list-sum (map factorial (number->digits n)))))    
    n)
  )

(define (run)
  (time-function euler34a)
  )

(run)





