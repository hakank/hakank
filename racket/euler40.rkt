#|
  Euler #40 in Racket

  """
  An irrational decimal fraction is created by concatenating the positive integers:
   
  0.123456789101112131415161718192021...
   
  It can be seen that the 12th digit of the fractional part is 1.

  If dn represents the nth digit of the fractional part, find the 
  value of the following expression.
  
  d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function integer-length2
                  ))

;;; A little too slow...
;;; cpu time: 325 real time: 325 gc time: 168
(define (euler40a)
  (let ([d (apply string-append (for/list ([i 1000000])
                                  (number->string i)
                                  ))])
    (for/product ([i (range 7)])
      (- (char->integer (string-ref d (expt 10 i) )) 48)
      )
    )
  )

;;; Calculate the numbers needed to get max-len (1000000) digits
(define (calc-max-num-40 max-len)
  (let ([s 0])
    (for/last ([i (in-naturals 1)]
               ;;; Order matters...
               #:break (> s (add1 max-len))
               #:do [(set! s (+ s (integer-length2 i))) ])
      i)
    )
  )

;;; Quite faster
;;; cpu time: 47 real time: 47 gc time: 15
(define (euler40b)
  (let ([d (apply string-append (for/list ([i (add1 (calc-max-num-40 1000000))])
                                  (number->string i)
                                  ))])
    (for/product ([i (range 7)])
      (- (char->integer (string-ref d (expt 10 i) )) 48)
      )
    )
  )

(define (run)
  ;;; (time-function euler40a)
  (time-function euler40b)
  )

(run)

