#|
  Euler #38 in Racket

  """
  Take the number 192 and multiply it by each of 1, 2, and 3:

      192 × 1 = 192
      192 × 2 = 384
      192 × 3 = 576

  By concatenating each product we get the 1 to 9 pandigital, 
  192384576. We will call 192384576 the concatenated product of 192 
  and (1,2,3)

  The same can be achieved by starting with 9 and multiplying by 
  1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the 
  concatenated product of 9 and (1,2,3,4,5).

  What is the largest 1 to 9 pandigital 9-digit number that can be 
  formed as the concatenated product of an integer with 
  (1,2, ... , n) where n > 1?
  """


  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require "utils_hakank.rkt")

;;; This is a port of my Picat program euler38.pi (euler38/0)
;;; A little too messy, though.
;;; cpu time: 5 real time: 5 gc time: 0
(define (euler38a)
  (first
   (filter number?
           (for/list ([n (range 9876 8 -1)])
             (let ([s (number->string n)])
               (for/first ([i (in-naturals 2)]
                           #:do [(set! s (string-append s (number->string (* n i)))) ]
                           #:break (> (string-length s) 9)
                           #:when (and (not (string-contains? s "0" )) (= (string-length s) 9) (pandigital s 9)))
                 (string->number s))
               )
             )
           )
   )
  )

    
(define (run)
  (time-function euler38a)
  )

(run)

