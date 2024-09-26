#| 

  Extreme value distribution (experimental) in Racket.Gamble 

  From Mathematica ExtremeValueDistribution
  """
  ExtremeValueDistribution can be used to model monthly maximum wind speeds. Recorded monthly 
  maxima of wind speeds in km/h for Boston, MA, from January 1950 till December 2009:
   ...
  Fit the distribution to the data:
  edist = EstimatedDistribution(maxWinds, ExtremeValueDistribution(a, b))
  -> ExtremeValueDistribution(47.2374, 9.1497))

  ... 

  Find the probability of monthly maximum wind speed exceeding 60 mph:
  Probability(x > Quantity(60, "mph"), x ~ edist)
  -> 0.00454842
  """

  Note that the original data was in km/h so let's keep that unit:
  """
  Probability(x > Quantity(60, "km per hour"), x ~ edist)
  -> 0.219536
  """


  var : d
  mean: 52.51102556055579
  Min: 26.685524967758766 Mean: 52.54417859631838 Max: 119.36836838762792 Variance: 135.38977004915907 Stddev: 11.635710981678733
  Credible interval (0.94): 33.54098230716407..74.63046501747999
  Percentiles::
  (0.01 33.52260420759886)
  (0.1 39.5979336920532)
  (0.025 35.52187695101051)
  (0.25 44.241164346234314)
  (0.5 50.670670849734265)
  (0.75 58.64987019180586)
  (0.84 63.265421452442865)
  (0.9 67.90897499330534)
  (0.975 80.63069430883036)
  (0.99 88.4943956954532)
  (0.999 105.94437475018043)

  var : p
  mean: 0.21830000000002353
  Min: 0 Mean: 0.2206 Max: 1 Variance: 0.17193564 Stddev: 0.41465122693656653
  Credible interval (0.94): 0..1
  Percentiles::
  (0.01 #f)
  (0.1 #f)
  (0.025 #f)
  (0.25 #f)
  (0.5 #f)
  (0.75 #f)
  (0.84 #t)
  (0.9 #t)
  (0.975 #t)
  (0.99 #t)
  (0.999 #t)


  Compare with gamble_extreme_value_dist.rkt

  This is a port of my WebPPL model extreme_value_test.wppl

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
   ; mh-sampler ; #:transition (slice)

   (define a 47.2374)
   (define b 9.1497)
   (define d (extreme_value_dist2 a b))
   (define p (> d 60))
    
   (list d
         p
    )

   
   )
  )

(show-marginals (model)
                (list  "d"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                #:show-percentiles? #t
                )
