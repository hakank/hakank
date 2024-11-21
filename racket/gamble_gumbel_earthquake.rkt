#| 

  Earthquake Extreme Value Theory using Gumbel distribution in Racket/Gamble 

  Example and data from Mathematica GumbelDistribution: yearly maximum magnitude 
  of earthquakes in the United States in the past 200 years:
  """
  mYdist = EstimatedDistribution(maxYearly, GumbelDistribution(Alpha, Beta))
  -> GumbelDistribution(7.03496, 0.745586)

  ...
  Find the probability of the annual maximum earthquake having a magnitude of at least 6:
  Probability(x >= 6, x -> mYdist)
  ->  0.779155

  Simulate the magnitudes of the annual maximum earthquake for 30 years:
  maxMag = RandomVariate(mYdist, 30)
  -> 
  (6.60781, 5.93618, 7.73324, 6.22724, 3.95527, 6.11688, 7.49837,
  6.53918, 8.12725, 5.62827, 7.77249, 6.60604, 7.20895, 6.6224,
  7.78966, 6.77433, 6.95503, 7.08528, 6.34501, 6.95825, 6.48127,
  7.54685, 7.37537, 6.6997, 7.07498, 7.34062, 6.74814, 7.24383,
  5.45639, 6.85268)

  Max@%
  -> 8.12725

  """

  (mean(yearly_max): 6.6198675496688795 stdev: 0.8819019082836784)
  var : mu
  mean: 6.737871077201903
  HPD interval (0.84): 6.027463485353741..7.551667605556131

  var : sigma
  mean: 0.475943959546934
  HPD interval (0.84): 0.0005414500163452708..0.7781609920453016

  var : post
  mean: 6.452901723678504
  HPD interval (0.84): 5.3845693548637446..7.570814872189579

  var : p
  mean: 0.7593025678794175

  var : post30-max
  mean: 7.471733208167047
  HPD interval (0.84): 6.236041474881337..8.155068987563576


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

;; This is from Mathematica GumbelDistribution:
;; The yearly maximum value of earthquakes in USA the past 200 year
(define yearly_max '(6.0 4.5 5.3 6.5 4.4 6.0 7.2 7.4 5.0 5.5 4.6 4.5 6.8 7.0
                         4.6 6.0 4.2 4.9 6.5 4.6 6.0 5.5 7.6 6.1 6.3 5.6 5.9 4.8 
                         5.9 6.3 5.8 5.1 7.9 6.1 6.0 7.0 7.3 6.7 5.8 4.8 6.2 5.8 
                         6.0 6.2 6.0 5.9 6.2 6.7 6.3 5.9 6.0 6.3 5.5 6.7 5.5 5.9 
                         5.9 6.0 6.4 7.6 8.0 7.7 7.1 7.0 7.0 7.3 7.4 7.8 7.4 7.0 7.4 
                         7.0 7.1 7.3 7.2 6.7 7.7 7.7 7.9 6.8 6.1 5.5 5.2 7.3 7.2 
                         5.7 6.7 7.0 7.1 6.8 7.8 6.5 6.2 7.2 6.9 7.1 7.1 6.2 7.3 
                         8.3 6.2 7.4 6.7 6.9 7.4 7.1 6.7 7.3 7.2 7.5 6.9 6.7 7.1 
                         7.2 7.1 7.2 7.0 6.8 8.1 7.9 7.7 6.9 6.7 6.7 6.7 8.4 8.2 
                         7.0 6.7 7.1 6.5 6.7 7.1 7.6 6.7 6.5 7.6 6.3 6.7 6.7 7.1 
                         6.9 7. 6.2 7.2 6.6 6.5 7.9 7.8 7.7 7.1))
(show2 "mean(yearly_max):" (avg yearly_max) "stdev:" (stddev yearly_max))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define mu (normal (avg yearly_max) (stddev yearly_max)))
   (define sigma (uniform 0 10))

   (define sigmaGauss 10)
   ; (define sigmaGauss (uniform 0 10))
    
   (for ([i (length yearly_max)])
     (observe-sample (normal-dist (gumbel_dist mu sigma) sigmaGauss) (list-ref yearly_max i)))

   (define post (gumbel_dist mu sigma))
   (define p (>= post 6))

   ; Generate 30 samples and find the max
   (define post30-max (max-list (for/list ([i 30]) (gumbel_dist mu sigma))))
   
   (list mu
         sigma
         post
         p
         post30-max
         ; sigmaGauss
         )
   )
)

(show-marginals (model)
                (list  "mu"
                       "sigma"
                       "post"
                       "p"
                       "post30-max"
                       ; "sigmaGauss"
                     )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


