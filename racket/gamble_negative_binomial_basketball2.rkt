#| 

  Negative binomial basketball II in Racket/Gamble 

  From Mathematica (NegativeBinomialDistribution)
  """
  Assume the probability of fouling for each minute interval is 0.1 independently. 
  Simulate the fouling process for 30 minutes

    RandomVariate(BernoulliDistribution(0.1), 30)
    -> 
    (0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0)

  A basketball player fouls out after 6 fouls. Find the expected playing time until foul out:
    NExpectation(x + 6, x -> NegativeBinomialDistribution(6, 0.1))
    -> 
    60
  """

  variable : v
  45: 0.019120000000000012
  47: 0.01903000000000001
  46: 0.01876000000000001
  42: 0.01845000000000001
  38: 0.01824000000000001
  ...
  198: 1.0000000000000006e-5
  199: 1.0000000000000006e-5
  202: 1.0000000000000006e-5
  203: 1.0000000000000006e-5
  221: 1.0000000000000006e-5
  mean: 54.00559000000004

  variable : time_until_foul_out
  51: 0.019120000000000012
  53: 0.01903000000000001
  52: 0.01876000000000001
  48: 0.01845000000000001
  44: 0.01824000000000001
  ...
  204: 1.0000000000000006e-5
  205: 1.0000000000000006e-5
  208: 1.0000000000000006e-5
  209: 1.0000000000000006e-5
  227: 1.0000000000000006e-5
  mean: 60.00559000000002


  Simulate fouling for 30 minutes:
  '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  negative_binomial_mean 6 0.1:: 54.0


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define v (negative_binomial 6 0.1))
   (define time_until_foul_out (+ v 6))
    
   (list v
         time_until_foul_out
         )
   )
)

(show-marginals (model)
                (list  "v"
                       "time_until_foul_out"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


(displayln "\nSimulate fouling for 30 minutes:")
(repeat (lambda () (bernoulli 0.1)) 30)
(show "negative_binomial_mean 6 0.1:" (negative_binomial_mean 6 0.1))
