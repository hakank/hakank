#| 

  Profits in Racket/Gamble 

  From "Resampling Stats Illustrations" (http://www.statistics101.net/PeterBruce_05-illus.pdf)
  Page 33
  """
  A confidence interval for a median
  (program “profits”)
  Sometimes, especially with highly skewed data such as incomes,
  the median is preferred to the mean as a measure of the distribu-
  tion center. Constructing a confidence interval for the median is
  easy with RESAMPLING STATS.
  Say you need to come up with a quick estimate of the profits of a
  typical American Fortune 1000 business, and the extent to which
  that estimate might be in error. You draw a random sample of 15
  firms, finding their profits (in $ million) to be: 1315, 288, 155, 37,
  99, 40, 170, 66, 500, 419, 125, -90, -63, 29, 966. We use
  RESAMPLING STATS to calculate the median profit, and construct
  a bootstrap confidence interval
  -> 
  median = 125
  interval = 40 288
  """

  I also included sample min, mean, and max.

  (min -90 mean: 270.4 median: 125 max 1315)

  variable : sample_median
  mean: 134.59810000001204
  HPD interval (0.9): 37.0..170.0
  HPD interval (0.95): 40.0..288.0
  HPD interval (0.99): 37.0..419.0
  Percentiles:
  (0.01 37.0)
  (0.025 37.0)
  (0.1 66.0)
  (0.05 40.0)
  (0.25 99.0)
  (0.5 125.0)
  (0.75 155.0)
  (0.84 170.0)
  (0.9 170.0)
  (0.95 288.0)
  (0.975 288.0)
  (0.99 419.0)
  (0.999 500.0)

  variable : sample_mean
  mean: 270.3698333333583
  HPD interval (0.9): 108.4..420.06666666666666
  HPD interval (0.95): 84.86666666666666..454.8
  HPD interval (0.99): 56.93333333333333..535.8666666666667
  Percentiles:
  (0.01 77.93333333333334)
  (0.025 100.66666666666667)
  (0.1 149.8)
  (0.05 121.8)
  (0.25 199.2)
  (0.5 262.46666666666664)
  (0.75 331.3333333333333)
  (0.84 365.4)
  (0.9 396.06666666666666)
  (0.95 436.8666666666667)
  (0.975 474.6666666666667)
  (0.99 520.0)
  (0.999 610.6666666666666)

  variable : sample_min
  mean: -69.16090000000081
  HPD interval (0.9): -90..29
  HPD interval (0.95): -90..29
  HPD interval (0.99): -90..37
  Percentiles:
  (0.01 -90)
  (0.025 -90)
  (0.1 -90)
  (0.05 -90)
  (0.25 -90)
  (0.5 -90)
  (0.75 -63)
  (0.84 -63)
  (0.9 29)
  (0.95 29)
  (0.975 37)
  (0.99 37)
  (0.999 66)

  variable : sample_max
  mean: 1129.8066000000217
  HPD interval (0.9): 500..1315
  HPD interval (0.95): 500..1315
  HPD interval (0.99): 419..1315
  Percentiles:
  (0.01 419)
  (0.025 419)
  (0.1 500)
  (0.05 500)
  (0.25 966)
  (0.5 1315)
  (0.75 1315)
  (0.84 1315)
  (0.9 1315)
  (0.95 1315)
  (0.975 1315)
  (0.99 1315)
  (0.999 1315)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define profits '(1315 288 155 37 99 40 170 66 500 419 125 -90 -63 29 966))
(show2 "min" (apply min profits) "mean:" (* 1.0 (avg profits)) "median:" (median profits) "max" (apply max profits))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define sample (resample-all profits))
   (define sample_median (median sample))
   (define sample_mean (avg sample))   
   (define sample_min (apply min sample))
   (define sample_max (apply max sample))   
         
   (list (* 1.0 sample_median)
         (* 1.0 sample_mean)
         sample_min
         sample_max
         )

   )
)

(show-marginals (model)
                (list  "sample_median"
                       "sample_mean"
                       "sample_min"
                       "sample_max"
                     )
              #:num-samples 10000
              #:truncate-output 5
              #:skip-marginals? #t
              ; #:show-stats? #t
              ; #:credible-interval 0.84
              #:hpd-interval (list 0.9 0.95 0.99)
              ; #:show-histogram? #t
              #:show-percentiles? #t
              ; #:burn 0
              ; #:thin 0
              )


