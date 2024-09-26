#| 

  BDA  in Racket Gamble.

  From https://github.com/probmods/ppaml2016/blob/gh-pages/chapters/5-data.md
  """
  Here, we explore the result of an experiment with 15 trials and binary outcomes 
  (e.g., flipping a coin with an uncertain weight, asking people if they'll vote for 
  Hillary Clinton or Donald Trump, ...) 
  """

  var : prior_p
  mean: 0.4969468261294316
  Credible interval (0.84): 0.1433567637154383..0.9750448769911506

  var : priorPredictive
  mean: 7.498648762556414
  Credible interval (0.84): 0..13

  var : posterior p
  mean: 0.11611697525917587
  Credible interval (0.84): 0.010992540811758603..0.1754335771524776

  var : posteriorPredictive
  mean: 1.775721845768046
  Credible interval (0.84): 0..3


  This is a port of my WebPPL model bda.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 15) ; number of attempts
   (define k 1)  ; number of successes
  

   (define p (uniform 0 1))

   ;; sample from binomial with updated p
   (define posteriorPredictive (binomial n p))

   ;; sample fresh p
   (define prior_p (uniform 0 1))
   ;; sample from binomial with fresh p
   (define priorPredictive (binomial n prior_p))

   ;; Observed k number of successes, assuming a binomial
   (observe-sample (binomial-dist n p) k)
   
   (list prior_p
         priorPredictive
         p
         posteriorPredictive
         )

   )
  )

(show-marginals (model)
                (list "prior_p"
                      "priorPredictive"
                      "posterior p"
                      "posteriorPredictive"
                      )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram #t
                ; #:show-percentile #t
                )

