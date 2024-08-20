#|
  Euler #36 in Racket

  """
  The decimal number, 585 = 1001001001_(2) (binary), is palindromic 
  in both bases.
  
  Find the sum of all numbers, less than one million, which are palindromic 
  in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not 
   include leading zeros.)
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function list-sum palindromic?
                  ))

;;; cpu time: 261 real time: 261 gc time: 98
(define (euler36a)
  (list-sum (for/list ([n (range 1 1000000)]
                       #:when (and (palindromic? (number->string n)) (palindromic? (~r n #:base 2))))
              n)
            )
  )

;;; Slower
;;; cpu time: 729 real time: 729 gc time: 97
(define (euler36b)
  (list-sum (for/list ([n (range 1 1000000)]
                       #:when (and (palindromic? (~r n #:base 10)) (palindromic? (~r n #:base 2))))
              n)
            )
  )

(define (run)
  (time-function euler36a)
  ;;; (time-function euler36b)
  )

(run)





