#|
  Euler #32 in Racket

  """
  We shall say that an n-digit number is pandigital if it makes use of 
  all the digits 1 to n exactly once; for example, the 5-digit number, 
  15234, is 1 through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
  containing multiplicand, multiplier, and product is 1 through 9 
  pandigital.

  Find the sum of all products whose multiplicand/multiplier/product 
  identity can be written as a 1 through 9 pandigital.
  HINT: Some products can be obtained in more than one way so be sure 
  to only include it once in your sum.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function list-sum pandigital
                  ))


;;; A little slow...
;;; cpu time: 220 real time: 220 gc time: 40
(define (euler32a)
  (list-sum
   (remove-duplicates
    (flatten
     ;;; I would rather use a for*/hash but then there's a lot of overhead
     (for/list ([a (range 2 99)])
       (let ([as (number->string a)])
         (for/list ([b (range (add1 a) 9876)]
                    ;;; Not sure if define is the best here, but 
                    ;;; let seems not to work.
                    #:do [(define p (number->string (* a b)))
                          (define ll (string-append as (number->string b) p))]
                    #:when (and (= 9 (string-length ll))
                                (not (string-contains? ll "0"))
                                (pandigital ll 9)))
           (string->number p))
         )
       ))))
  )

;;; Using for*/hash, a little slower (as expected)
;;; cpu time: 269 real time: 269 gc time: 57
(define (euler32b)
  (list-sum
   (hash-keys (for*/hash ([a (range 2 99)]
                          [b (range (add1 a) 9876)]
                          #:do [(define p (number->string (* a b)))
                                (define ll (string-append (number->string a)
                                                          (number->string b)
                                                          p))]
                          #:when (and (= 9 (string-length ll))
                                      (not (string-contains? ll "0"))
                                      (pandigital ll 9)))
                (values (string->number p) 1)
                )
              )
   )
  )

(define (run)
  (time-function euler32a)
  ;;; (time-function euler32b)
  )

(run)




