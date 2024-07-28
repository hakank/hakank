#|
  Euler #41 in Racket

  """
  We shall say that an n-digit number is pandigital if it makes use of all 
  the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
  and is also prime.

  What is the largest n-digit pandigital prime that exists?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in  math/number-theory prime?))
;;; (require racket/trace)

(require "utils_hakank.rkt")

;;; Simplification:
;;; n=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
;;; n=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3
;;;
;;; cpu time: 23 real time: 23 gc time: 2
;;; It's a pity the permutations is not lexicographic
(define (euler41a)
  (apply max (flatten (for/first ([n (range 7 0 -1)])
    (for/list ([p (permutations (range 1 (add1 n)))]
               #:do [(define pn (digits->number p))]
               #:when (prime? pn))
      pn)
    )))
  )

;;; Slightly changed variant, same time
;;; cpu time: 23 real time: 23 gc time: 2
(define (euler41b)
  (for/first ([n (range 7 0 -1)])
    (apply max (flatten (for/list ([p (permutations (range 1 (add1 n)))]
               #:do [(define pn (digits->number p))]
               #:when (prime? pn))
      pn)
    )))

 )

;;; Faster (and neater) with pre-processing to get lexicographic order of the
;;; permutations (as numbers, not digits).
;;; cpu time: 3 real time: 3 gc time: 0
(define (euler41c)
  (for*/first ([n (range 7 0 -1)]
               [p (permutations-numbers-lex (range 1 (add1 n)) >)]
               #:when (prime? p))
    p)
  )


(define (run)
  ;;; (time-function euler41a)
  ;;; (time-function euler41b)
  (time-function euler41c)
  )

(run)
