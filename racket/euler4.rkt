#|
  Euler #4 in Racket

  Problem 4
  """
  A palindromic number reads the same both ways. The largest palindrome made 
  from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;; (require math/number-theory)

(require (only-in "utils_hakank.rkt"
                  time-function palindromic?
                  ))

;;; cpu time: 74 real time: 74 gc time: 11
(define (euler4a)
  (first (sort (for*/list ([a (range 100 1000)]
                           [b (range a 1000)]
                           #:when (palindromic? (string->list (number->string (* a b))) ))
                 (* a b))
               >))
  )

;;; cpu time: 79 real time: 79 gc time: 16
(define (euler4b)
  (let ([max-val 0])
    (for* ([a (range 100 1000)]
           [b (range a 1000)]
           #:when (palindromic? (string->list (number->string (* a b))) ))
      (let ([ab (* a b)])
            (when (> ab max-val)
              (set! max-val ab)))
      )
    max-val)
  )

(define (run) 
  (time-function euler4a)
  ;;; (time-function euler4b)
  )

(run)

