#|
  Euler #49 in Racket

  """
  The prime 41, can be written as the sum of six consecutive primes:
  41 = 2 + 3 + 5 + 7 + 11 + 13

  This is the longest sum of consecutive primes that adds to a prime 
  below one-hundred.

  The longest sum of consecutive primes below one-thousand that adds to a prime, 
  contains 21 terms, and is equal to 953.
  
  Which prime, below one-million, can be written as the sum of the most 
  consecutive primes?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (except-in math/number-theory permutations))
;;; (require racket/trace)

(require "utils_hakank.rkt")

(define (euler50a)
  (let ([ps (primes 1000000)]
        [found #f])
    (for ([len (range 550 20 -1)])
      #:break (not (equal? found #f))
      (for ([offset (range 1 550)]
            #:break (not (equal? found #f)))
        (let ([pp (list-sum
                   (for/list ([j (range (add1 offset) (add1 (+ offset len)))])
                     (list-ref ps j)))])
          (when (and (< pp 1000000) (prime? pp))
            (set! found pp))
          )
        )
      )
    found)
  )

(define (run) 
  (time-function euler50a)
  )

(run)
