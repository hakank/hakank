#|
  Euler #30 in Racket

  """
  Surprisingly there are only three numbers that can be written 
  as the sum of fourth powers of their digits:

     1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
     8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
     9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

  As 1 = 1^(4) is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of 
  fifth powers of their digits.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function list-sum number->digits
                  ))

;;; cpu time: 132 real time: 132 gc time: 36
(define (euler30a)
  (list-sum
   (for/list ([n (range 10 (* 6 (expt 9 5)))]
              #:when (= n (list-sum (map (lambda (i) (expt i 5)) (number->digits n)))))
     n))
  )

(define (run)
  (time-function euler30a)
  )

(run)




