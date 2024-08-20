#|
  Euler #15 in Racket

  Problem 15
  """
  Starting in the top left corner of a 2×2 grid, there are 6 routes 
  (without backtracking) to the bottom right corner.
  
  How many routes are there through a 20×20 grid?
  """


  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)

(require (only-in "utils_hakank.rkt"
                  time-function list-product
                  ))

;;; cpu time: 0 real time: 0 gc time: 0
(define (euler15a)
  (/ (list-product (range 21 41)) (list-product (range 2 21)))
  )

(define (run)
  (time-function euler15a)
  )

(run)


