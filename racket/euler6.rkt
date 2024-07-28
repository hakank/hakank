#|
  Euler #6 in Racket

  Problem 6
  """
  The sum of the squares of the first ten natural numbers is,
  1^(2) + 2^(2) + ... + 10^(2) = 385

  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

  Hence the difference between the sum of the squares of the first ten 
  natural numbers and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one 
  hundred natural numbers and the square of the sum.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require math)

(require "utils_hakank.rkt")

;;; cpu time: 0 real time: 0 gc time: 0
(define (euler6a)
  (let ([s1 (sum (map (λ (i) (expt i 2)) (range 1 101)))]
        [s2 (expt (sum (range 1 101)) 2)])
    (- s2 s1)
  ))

(define (expt2 n)
  (expt n 2)
  )

;; using expt2, slightly shorter
;;; cpu time: 0 real time: 0 gc time: 0
(define (euler6b)
  (let ([s1 (sum (map expt2 (range 1 101)))]
        [s2 (expt2 (sum (range 1 101)))])
    (- s2 s1)
  ))

;; Silly variant
;; cpu time: 0 real time: 0 gc time: 0
(define (euler6c)
  (- (expt2 (sum (range 1 101)))
     (for/sum ([i (range 1 101)])(expt2 i)))
  )

(define (run)
  ;; (time-function euler6a)
  ;; (time-function euler6b)
  (time-function euler6c)
  )

(run)
