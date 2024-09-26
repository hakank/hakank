#| 

  Noisy Or in Racket.Gamble 

  From https://github.com/SHoltzen/dice/blob/master/benchmarks/baselines/noisyOr.psi
  https://github.com/SHoltzen/dice/blob/master/benchmarks/baselines/noisyOr.dice

  var : n3
  #t: 130307/160000 (0.81441875)
  #f: 29693/160000 (0.18558125)
  mean: 130307/160000 (0.81441875)

  var : expected
  130307/160000: 130307/160000 (0.81441875)
  29693/160000: 29693/160000 (0.18558125)
  mean: 8930794249/12800000000 (0.697718300703125)


  This is a port of my WebPPL model noisyOr.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate ; #:limit 1e-01
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n0  (flip 1/2))
   (define n4  (flip 1/2))
   (define n1  (if n0 (flip 4/5) (flip 1/10)))
   (define n21 (if n0 (flip 4/5) (flip 1/10)))
   
   (define n22 (if n4 (flip 4/5) (flip 1/10)))
   (define n33 (if n4 (flip 4/5) (flip 1/10)))
   
   (define n2  (or n21 n22))
   (define n31 (if n1 (flip 4/5) (flip 1/10)))
   (define n32 (if n2 (flip 4/5) (flip 1/10)))
    
   (define n3 (or n31 n32 n33 ))
   
   ;; return n3 ;; PSI:  expected: 130307/160000·δ(1)(n3)+29693/160000·δ(0)(n3)
   (list n3
         (+ (* (/ 130307 160000) (if n3 1 0)) (* (/ 29693 160000) (if n3 0 1))) ;; the expected value
    )

   )
)

(show-marginals (model)
                (list  "n3"
                       "expected"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


