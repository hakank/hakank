#|
  Euler #47 in Racket

  """
  The first two consecutive numbers to have two distinct prime factors are:

  14 = 2 x 7
  15 = 3 x 5

  The first three consecutive numbers to have three distinct 
  prime factors are:

  644 = 2^2 x 7 x 23
  645 = 3 x 5 x 43
  646 = 2 x 17 x 19.

  Find the first four consecutive integers to have four distinct primes 
  factors. What is the first of these numbers?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (except-in math/number-theory permutations))
;;; (require racket/trace)

(require "utils_hakank.rkt")

;;; cpu time: 40 real time: 40 gc time: 7
(define (euler47a)
  (let* ([max-n 1000000]
         [f (make-vector max-n 0)]
         [goal '(4 4 4 4)]
         [found #f])
    (for ([i (range 2 max-n)])
      (when (= (vector-ref f i) 0)
        (for ([j (range (* i 2) max-n i)])
          (vector-set! f j (add1 (vector-ref f j))))
        )
      )
    (for ([i (range 2 (- max-n 3))]
          #:break (not (equal? found #f))
          #:when (equal? (vector-slice f i 4) goal) 
          ;;; #:when (equal? (for/list ([k (range 4)]) (vector-ref f (+ i k)))  goal)
          )
      (set! found i)
      )   
    found
    )
  )

(define (run) 
  (time-function euler47a)
  )

(run)
