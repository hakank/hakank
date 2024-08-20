#|
  Euler #35 in Racket

  """
  The number, 197, is called a circular prime because all rotations 
  of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 
  2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in math/number-theory
                  prime?
                  ))

;;; (require racket/trace)
;;; (require memoize) 

;;; This seems to be too messy 
;;; (require seq/base) ;;; https://docs.racket-lang.org/seq/  Must be installed

(require (only-in "utils_hakank.rkt"
                  time-function digits->number rotate primes number->digits
                  ))

(define (check35 digits ht)
  (let ([len (length digits)])
    (for/and ([i (range 1 len) ])
      (hash-has-key? ht (digits->number (rotate digits i)))
      )
    )
  )

;;; cpu time: 170 real time: 170 gc time: 45
(define (euler35a)  
  (let ([ht (for/hash ([p (primes 1000000)]) (values p 1))])
    (length (for/list ([p (hash-keys ht)]
               #:when (check35 (number->digits p) ht))
      p
      ))
    )
  
  )

;;; Test to memoize
;;; Not faster using this in check-b
;; (define/memo (prime?-memo n)
;;   (prime? n))


;;; Just use prime?, i.e. no hash
;;; (define/memo (check-b digits) ;;; slower
(define (check35-b digits)
  (let ([len (length digits)])
    (for/and ([i (range 1 len) ])
      (prime? (digits->number (rotate digits i)))
      )
    )
  )



;;; Just use primes and prime? Slightly slower.
;;; cpu time: 162 real time: 162 gc time: 3
(define (euler35b)  
  (length (for/list ([p (primes 1000000)]
    #:when (check35-b (number->digits p)))
    p))
  
  )

(define (run)
  (time-function euler35a)
  ;;; (time-function euler35b)
  )

(run)





