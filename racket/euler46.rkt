#|
  Euler #46 in Racket

  """
  It was proposed by Christian Goldbach that every odd composite number can be 
  written as the sum of a prime and twice a square.

  9 = 7 + 2×1^2
  15 = 7 + 2×2^2
  21 = 3 + 2×3^2
  25 = 7 + 2×3^2
  27 = 19 + 2×2^2
  33 = 31 + 2×1^2

  It turns out that the conjecture was false.

  What is the smallest odd composite that cannot be written as the 
  sum of a prime and twice a square?
  """

  TODO: Perhaps a little too messy.

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in math/number-theory
                  prime?))
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function
                  ))

;;; A little too messy
;;; cpu time: 21 real time: 21 gc time: 2
(define (euler46a)
  (let ([res 0]
        [got-it #f]
        [found-it #f])
    ;;; i are the composite numbers (i.e. no primes)
    (for ([i (filter (lambda (n) (not (prime? n))) (range 3 10001 2))]
                #:do [(define s (inexact->exact (round (sqrt (/ i 2)))))
                      (set! found-it #f)]
                #:break (equal? got-it #t))
      ;;; Check if we can find an i that can not be so combined.
      (for ([j (range 1 (add1 s))]
            #:break (equal? found-it #t))
        (when (prime? (abs (- i (* j j 2))))
          (set! found-it #t)))
      ;;; Check if we found no such composite number
      (when (equal? found-it #f)
        (set! res i)
        (set! got-it #t))
      )
      
    res)
  )

(define (run) 
  (time-function euler46a)
  )

(run)
