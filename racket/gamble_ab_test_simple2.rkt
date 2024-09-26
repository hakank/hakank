#| 

  A/B test simple 2 in Racket.Gamble 

  var : rateA
  mean: 0.6319563097918639

  var : rateB
  mean: 0.7839976714512473

  var : diff
  mean: 0.152041361659384

  var : diff > 0.0
  mean: 0.9370000000000007

  var : p
  mean: 0.06300000000000003


  This is a port of my WebPPL model ab_test_simple2.wppl

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

   (define nA 25)  ; number of trial for A    
   (define nB 72)  ; number of trial for B

   (define rateA (beta 1 1))
   (define rateB (beta 1 1))
    
   (define sA (binomial nA rateA))
   (define sB (binomial nB rateB))
    
   (define diff (- rateB rateA))
   (define p (> rateA rateB))
    
   (observe/fail (= sA 16)) ; number of successes for A
   (observe/fail (= sB 57)) ; number of successes for B
    
   (list rateA
         rateB
         diff
         (> diff 0.0)
         p
    )

   )
)

(show-marginals (model)
                (list  "rateA"
                       "rateB"
                       "diff"
                       "diff > 0.0"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


