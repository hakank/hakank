#| 

  Bayesian null hypothesis test in Racket Gamble.

  From https://mhtess.github.io/bdappl/chapters/04-hypothesisTesting.html
  """
  '''
  Let’s see an example: Consider our children helping study from the previous study. 
  Rather than estimate the propensity_to_help, we might want to perform a 
  "Null Hypothesis" test: What is the probability that the behavior observed in 
  our experiment (that 15 out of 20 kids helped) could be explained by true 
  randomness (i.e., propensity_to_help = 0.5)?  
 
  ...
 
  Bayesian Null Hypothesis Testing requires you to specify both hypotheses: 
  It is not enough to define what the null hypothesis is. The alternative 
  hypothesis is intuitively M1:θ≠0.5 but we must be more specific. One standard 
  way of defining the null hypothesis is to put say that the parameter is 
  drawn from a uniform distribution.(Note that this was the model from the 
  previous chapter.)
  """

  var : the_better_model
  alternative: 0.7562855644344214
  null: 0.2437144355655786


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; number of observed successes, total number of trials
(define k 15)
(define n 20)

(define null_model (dist-unit 0.5)) ; null hypothesis is a Delta distribution at 0.5
(define alternative_model (uniform-dist 0 1))


(define (model)
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ;; binary decision variable for which hypothesis is better
   (define the_better_model (uniform-draw (list "null" "alternative")))
   
   (define p (if (eq? the_better_model "null")
                 null_model
                 alternative_model))
  
   (observe-sample (binomial-dist n (sample p)) k)
   
   (list the_better_model)
   

   )
  )

(show-marginals (model)
                (list "the_better_model"
                      )
                #:num-samples 10000
                #:truncate-output 5
                #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram #t
                ; #:show-percentile #t
                )

