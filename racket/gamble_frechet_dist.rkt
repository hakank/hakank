#| 

  Frechet dist in Racket/Gamble 

  From Mathematica (FrechetDistribution)
  """
  Quantile(FrechetDistribution(alpha, beta), x)
  -> 
  beta*(-Log(x))^(-1/alpha)    0 < x < 1
  0                             x <= 0
  Infinity                      True
  """
      
  Example (op.cit)
  """
  According to a study, the annual maximal tephra (solid material) volume in volcanic eruptions 
  follows a FrechetDistribution with shape parameter 0.71 and scale parameter 6.3, given in cubic 
  kilometers:
  D = FrechetDistribution(0.71,6.3)
  ...
  Find the probability that the annual maximal tephra volume is greater than 30 cubic kilometers:
  Probability(x > 30,  Element(x, D))
  -> 0.281219
  ""

  var : d
  0.8358581449946353: 0.00010000000000000938
  8.448478646849237: 0.00010000000000000938
  81.59395683863086: 0.00010000000000000938
  1.731558386310215: 0.00010000000000000938
  1.8148406197351306: 0.00010000000000000938
  ...
  2.8994572235431657: 0.00010000000000000938
  9.662103682251821: 0.00010000000000000938
  5.90616134838937: 0.00010000000000000938
  11.646559958007586: 0.00010000000000000938
  5.288661284388919: 0.00010000000000000938
  mean: 5592.277954337743
  Min: 0.37287446710500444 Mean: 5471.4179837868805 Max: 23242513.18412138 Variance: 97876592613.56923 Stddev: 312852.3495413918
  Credible interval (0.84): 0.37287446710500444..73.51783654141711

  var : p
  #f: 0.7141999999999777
  #t: 0.28580000000002487
  mean: 0.28580000000002487
  Min: 0 Mean: 0.2887 Max: 1 Variance: 0.20535231 Stddev: 0.4531581512011011

  (sample: 6.239358473099865)
  (pdf: 0.02643526124451058)
  (cdf: 0.7608529556515563)
  (quantile: 7.80593759842106)
  (mean: 13.451693556415941)

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

   (define alpha 0.71)
   (define beta 6.3)
   (define d (frechet_dist alpha beta))

   (define p (> d 30))
    
   (list d
         p
    )
   )
)

(show-marginals (model)
                (list  "d"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(let ([alpha 1.71]
      [beta 6.3])
  (show2 "sample:" (frechet_dist alpha beta))
  (show2 "pdf:" (frechet_dist_pdf alpha beta 13.451693556415941))
  (show2 "cdf:" (frechet_dist_cdf alpha beta 13.451693556415941))
  (show2 "quantile:" (frechet_dist_quantile alpha beta 0.5))    
  (show2 "mean:" (frechet_dist_mean alpha beta))
)
