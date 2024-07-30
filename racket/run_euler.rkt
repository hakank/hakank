#|
  Run all the first 50 Project Euler programs  in Racket

  For this to work, the instances must define a run function which calls
  the specific solution.

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(require "utils_hakank.rkt")

(define (run-euler)
  (for ([n (range1 1 50)])
    (let ([e (format "euler~a.rkt" n)])
      (writeln e)
      (dynamic-require e 'run)
      (flush-output)
      )
    )
  #t
  )


(time-function run-euler)
