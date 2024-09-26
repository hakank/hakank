#| 

  Inverse exponential distr in Racket/Gamble 

  From Handbook on probability distributions
  page 60ff

  var : d1
  mean: 0.2664812575655873
  Min: 0.0005581020845981731 Mean: 0.2445981236460591 Max: 1.450191890864342 Variance: 0.05546496120603369 Stddev: 0.23551000234816713
  Credible interval (0.84): 0.0005581020845981731..0.4507365847646491

  var : d2
  mean: 0.23901832476962973
  Min: 0.00039682077382927706 Mean: 0.2412964569935237 Max: 1.5967378208470433 Variance: 0.05876156492011414 Stddev: 0.24240784830552442
  Credible interval (0.84): 0.00039682077382927706..0.4324259244689304

  See gamble_distributions.rkt and gamble_distributions_test.rkt

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
   
   (define lambda_ 1/4)
   (define d1 (inverse_exponential lambda_))
   (define d2 (exponential_dist (/ 1 lambda_)))
   
   (list d1
         d2
         )
   )
)

(show-marginals (model)
                (list  "d1"
                       "d2"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


