#| 

  Earthquake Extreme Value Theory using Weibull distribution in Racket/Gamble 

  Example and data from Mathematica WeibullDistribution: yearly maximum magnitude 
  of earthquakes in the United States in the past 200 years:
  """
  mYdist = EstimatedDistribution(maxYearly, WeibullDistribution(Alpha, Beta))
  -> WeibullDistribution(9.11753, 6.99072)

  Using the model, find the probability of the annual maximum earthquake of magnitude 
  at least 6:
  NProbability(x >= 6, x ~ mYdist)
  -> 0.780175

  Find the average magnitude of the annual maximum earthquake:
  Mean(mYdist)
  -> 6.62384
  """

  variable : a
  mean: 6.908696321629823

  variable : b
  mean: 11.120719479223089

  variable : post
  mean: 6.398126085274126

  variable : p
  mean: 0.7601999999999999

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; This is from Mathematica WeibullDistribution,
; it the yearly maximum value of earthquakes in USA the past 200 year
(define yearly_max '(6.0 4.5 5.3 6.5 4.4 6.0 7.2 7.4 5.0 5.5 4.6 4.5 6.8 7.
                         4.6 6.0 4.2 4.9 6.5 4.6 6.0 5.5 7.6 6.1 6.3 5.6 5.9 4.8 
                         5.9 6.3 5.8 5.1 7.9 6.1 6.0 7.0 7.3 6.7 5.8 4.8 6.2 5.8 
                         6.0 6.2 6.0 5.9 6.2 6.7 6.3 5.9 6.0 6.3 5.5 6.7 5.5 5.9 
                         5.9 6.0 6.4 7.6 8.0 7.7 7.1 7.0 7.0 7.3 7.4 7.8 7.4 7.0 7.4 
                         7.0 7.1 7.3 7.2 6.7 7.7 7.7 7.9 6.8 6.1 5.5 5.2 7.3 7.2 
                         5.7 6.7 7.0 7.1 6.8 7.8 6.5 6.2 7.2 6.9 7.1 7.1 6.2 7.3 
                         8.3 6.2 7.4 6.7 6.9 7.4 7.1 6.7 7.3 7.2 7.5 6.9 6.7 7.1 
                         7.2 7.1 7.2 7.0 6.8 8.1 7.9 7.7 6.9 6.7 6.7 6.7 8.4 8.2 
                         7.0 6.7 7.1 6.5 6.7 7.1 7.6 6.7 6.5 7.6 6.3 6.7 6.7 7.1 
                         6.9 7.0 6.2 7.2 6.6 6.5 7.9 7.8 7.7 7.1))
(show "mean(yearly_max)" (avg yearly_max))

(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler #:transition (slice)

   (define a (uniform 0 20))
   (define b (uniform 0 20))
    
   (for ([i (length yearly_max)])
     (observe-sample (normal-dist (weibull a b) 0.5) (list-ref yearly_max i)))
   
   (define post (weibull a b))
   (define p (>= post 6))
   
   (list a
         b
         post
         p
         )

   )
)

(show-marginals (model)
                (list  "a"
                       "b"
                       "post"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 10
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                #:burn 1000
                #:thin 10
                )


