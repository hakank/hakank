#|
  Euler #33 in Racket

  """
  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in 
  attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, 
  is obtained by cancelling the 9s.

  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

  There are exactly four non-trivial examples of this type of fraction, less than 
  one in value, and containing two digits in the numerator and denominator.

  If the product of these four fractions is given in its lowest common terms, find 
  the value of the denominator.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function
                  ))

;;; cpu time: 0 real time: 0 gc time: 0
(define (euler33a)
  (let ([s 1])
    (for ([y (range 1 10)])
      (for ([z (range 1 10)])
        (let ([x (/ (* 9.0 y z) (- (* 10.0 y) z))])
          (when (and
                   (= (* 1.0 (floor x)) (* x 1.0))
                   (< (/ y z) 1.0)
                   (< x 10.0))
            (set! s (* 1.0 (/ (* s y) z)))
            )
          )
        )
      )
    (/ 1 s)
    )
  )

;;; cpu time: 0 real time: 0 gc time: 0
(define (euler33b)
  (let ([s 1])
    (for* ([y (range 1 10)]
           [z (range 1 10)]
           #:do [ (define x (/ (* 9.0 y z) (- (* 10.0 y) z))) ]
           #:when (and
                   (= (* 1.0 (floor x)) (* x 1.0))
                   (< (/ y z) 1.0)
                   (< x 10.0)))
            (set! s (* 1.0 (/ (* s y) z)))
            )
    (/ 1 s) )
  )

(define (run)
  ;;; (time-function euler33a)
  (time-function euler33b)
  )

(run)





