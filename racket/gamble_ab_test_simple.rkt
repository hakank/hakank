#| 

  A/B test simple in Racket.Gamble 

  var : rateA
  mean: 0.09147315721055431

  var : rateB
  mean: 0.07344742808187465

  var : diff
  mean: -0.018025729128679738

  var : diff > 0.0
  mean: 0.4140000000000003

  This is a port of my WebPPL model ab_test_simple.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define nA 20)  ; number of trial for A    
   (define nB 25)  ; number of trial for B

   (define rateA (beta 1 1))
   (define rateB (beta 1 1))
    
   (define sA (binomial nA rateA))
   (define sB (binomial nB rateB))
    
   (define diff (- rateB rateA))
    
   (observe/fail (= sA 1)) ; number of successes for A
   (observe/fail (= sB 1)) ; number of successes for B
    
   (list rateA
         rateB
         diff
         (> diff 0.0)
    )

   )
)

(show-marginals (model)
                (list  "rateA"
                       "rateB"
                       "diff"
                       "diff > 0.0"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


