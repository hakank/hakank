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

(require (only-in math/number-theory
                  prime? next-primes
                  ))

(require (only-in "utils_hakank.rkt"
                  time-function list-sum primes
                  ))

;;; Too slow!
;;; cpu time: 1418 real time: 1420 gc time: 30
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

;;; Added #:break (> pp 1000000): much faster then euler50a
;;; cpu time: 38 real time: 38 gc time: 11
(define (euler50b)
  (let ([ps (next-primes 1 10000)]
        [found #f])
    (for ([len (range 550 20 -1)])
      #:break (not (equal? found #f))
      (for ([offset (range 1 550)]
            #:break (not (equal? found #f))
            #:do  [(define pp (list-sum
                               (for/list ([j (range offset (+ offset len))])
                                 (list-ref ps j))))
                   ]
            #:break (> pp 1000000)
            #:when (prime? pp))
        (set! found pp))
      )
    found)
  )

;;; Hashing the primes: not faster than euler50b
;;; cpu time: 69 real time: 69 gc time: 22
(define (euler50c)
  (let* ([ps (primes 1000000)]
         [ph (for/hash ([p ps]) 
               (values p 1))]
         [found #f])
    (for ([len (range 550 20 -1)])
      #:break (not (equal? found #f))
      (for ([offset (range 1 550)]
            #:break (not (equal? found #f))
            #:do  [(define pp (list-sum
                               (for/list ([j (range (add1 offset) (add1 (+ offset len)))])
                                 (list-ref ps j))))
                   ]
            #:break (> pp 1000000)            
            #:when (hash-has-key? ph pp))
        (set! found pp))
      )
    found)
  )

(define (run) 
  ;;; (time-function euler50a)
  (time-function euler50b)
  ;;; (time-function euler50c) 
  )

(run)
