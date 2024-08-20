#|
  Euler #26 in Racket

  Problem 26
  """
  A unit fraction contains 1 in the numerator. The decimal representation of the 
  unit fractions with denominators 2 to 10 are given:

      1/2	= 	0.5
      1/3	= 	0.(3)
      1/4	= 	0.25
      1/5	= 	0.2
      1/6	= 	0.1(6)
      1/7	= 	0.(142857)
      1/8	= 	0.125
      1/9	= 	0.(1)
      1/10	= 	0.1

  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be 
  seen that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in 
  its decimal fraction part.
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

(require (only-in "utils_hakank.rkt"
                  time-function
                  ))

;;;
;;; This is a port of my Picat program in euler26.pi
;;; 
(define (rep-len i)
  (let ([found-remainders (make-vector i 0)]
        [value 1]
        [position 1])
    (for/first ([tmp (in-naturals 1)]
                #:do [(vector-set! found-remainders value position)
                      (set! value (modulo (* value 10) i))
                      (set! position (add1 position))
                      ]
                #:when (or (not (= (vector-ref found-remainders value) 0 )) (= value 0)))
      value)
    (- position (vector-ref found-remainders value))
    )
  )

;;; cpu time: 0 real time: 0 gc time: 0
(define (euler26a)
  (second (first (sort (for/list ([d (range 2 1000)]
                                  #:when (prime? d))
                         (list (rep-len d) d)
                         ) > #:key first ))
          )
  )

(define (run)
  (time-function euler26a)
  )

(run)



