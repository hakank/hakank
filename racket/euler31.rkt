#|
  Euler #31 in Racket

  """
  In England the currency is made up of pound, £, and pence, p, and 
  there are eight coins in general circulation:

     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

  It is possible to make £2 in the following way:

     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

  How many different ways can £2 be made using any number of coins?

  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require "utils_hakank.rkt")

(define (coins cs money m)
  (if (<= money 0)
      0
      (let ([sum 0]
            [len (length cs)])
        (if (= m len)
            (set! sum 1)
            (for ([i (range m (add1 len))])
              (let ([ci (list-ref cs (sub1 i))])
                (when (= (- money ci) 0)
                  (set! sum (add1 sum)))
                (when (> (- money ci) 0)
                  (set! sum (+ sum (coins cs (- money ci) i)) )
                  )
                )
              )
            )
        sum)
      )
  )

;;; cpu time: 3 real time: 3 gc time: 0
(define (euler31a)
  (coins '(200 100 50 20 10 5 2 1) 200 1)
  )

(define (run)
  (time-function euler31a)
  )

(run)



