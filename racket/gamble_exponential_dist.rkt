#| 

  Exponential dist in Racket/Gamble 

  From Handbook on probability distributions
  page 56ff

  From Mathematica's ExponentialDistribution
  """
  A battery has a lifespan that is exponentially distributed with rate 
  parameter 1/3000 per hour. Find the probability that a random battery has a 
  lifespan of less than 2500 hours:
  
  Probability[t < 2500.0, t e ExponentialDistribution[1/3000]]
  -> 0.565402
  """

  var : d1
  4977.891965143857: 0.0009999999999999994
  2231.4278120884896: 0.0009999999999999994
  2228.785448820013: 0.0009999999999999994
  1712.0292893572857: 0.0009999999999999994
  4814.84039857326: 0.0009999999999999994
  ...
  2766.8984206797163: 0.0009999999999999994
  175.30172389163744: 0.0009999999999999994
  1063.3500930013695: 0.0009999999999999994
  3812.6122496874013: 0.0009999999999999994
  4431.222971537473: 0.0009999999999999994
  mean: 2809.9292384786463
  Min: 3.1745214792027356 Mean: 2854.8774541443922 Max: 19067.682974675987 Variance: 8390577.131107211 Stddev: 2896.6492937715484
  Credible interval (0.84): 3.1745214792027356..5092.812198964518

  var : d2
  1054.6841016254289: 0.0009999999999999994
  3654.6768701166707: 0.0009999999999999994
  1018.9298348860166: 0.0009999999999999994
  1205.7810440296935: 0.0009999999999999994
  392.32021893044515: 0.0009999999999999994
  ...
  645.1886398812488: 0.0009999999999999994
  620.0763462708751: 0.0009999999999999994
  8196.172104269057: 0.0009999999999999994
  2109.044659223056: 0.0009999999999999994
  588.7961467864061: 0.0009999999999999994
  mean: 2895.0331327788363
  Min: 1.488012266241057 Mean: 3163.40130897102 Max: 26494.674750662773 Variance: 9593292.250740984 Stddev: 3097.3040294328525
  Credible interval (0.84): 1.488012266241057..5648.167800022802

  var : d3
  6309.166195000833: 0.0009999999999999994
  498.0706534024257: 0.0009999999999999994
  818.9085328446255: 0.0009999999999999994
  514.3205608713743: 0.0009999999999999994
  2899.248007791678: 0.0009999999999999994
  ...
  861.4061353659695: 0.0009999999999999994
  1648.0408919287352: 0.0009999999999999994
  9164.669347703013: 0.0009999999999999994
  4307.836200353405: 0.0009999999999999994
  8462.176975022694: 0.0009999999999999994
  mean: 2967.9077921842713
  Min: 5.06402940955732 Mean: 3040.8302381747603 Max: 22972.949110815323 Variance: 9790511.545236306 Stddev: 3128.9793136478715
  Credible interval (0.84): 5.06402940955732..5504.517721389985

  var : p1
  #t: 0.6060000000000004
  #f: 0.3940000000000003
  mean: 0.6060000000000004
  Min: 0 Mean: 0.586 Max: 1 Variance: 0.242604 Stddev: 0.4925484747717731

  var : p2
  #t: 0.5900000000000004
  #f: 0.4100000000000003
  mean: 0.5900000000000004
  Min: 0 Mean: 0.532 Max: 1 Variance: 0.248976 Stddev: 0.498974949271003

  var : p3
  #t: 0.5790000000000004
  #f: 0.4210000000000003
  mean: 0.5790000000000004
  Min: 0 Mean: 0.565 Max: 1 Variance: 0.245775 Stddev: 0.4957569969249047

  PDF:
  0.0001448660695023594
  0.00014486606950235939

  CDF:
  0.5654017914929218
  0.5654017914929218

  Quantile:
  2500.0
  2500.0

  For more on this, see gamble_distributions.rkt and gamble_distributions_test.rkt

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

   (define lambda_ 3000) 
   (define d1 (exponential lambda_)) ; built-in
   (define d2 (exponential_dist (/ 1 lambda_)))
   (define d3 (exponential_dist2 (/ 1 lambda_)))

   ; This is also tested with exponential_dist_cdf below
   (define p1 (< d1 2500))
   (define p2 (< d2 2500))
   (define p3 (< d3 2500))
   
   (list d1
         d2
         d3
         p1
         p2
         p3
    )
   )
)

(show-marginals (model)
                (list  "d1"
                       "d2"
                       "d3"
                       "p1"
                       "p2"
                       "p3"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


;
; Comparing PDF, CDF, and quantiles with the built-in
;
(displayln "PDF:")
(dist-pdf (exponential-dist 3000) 2500)
(exponential_dist_pdf 1/3000 2500)
(newline)
(displayln "CDF:")
(dist-cdf (exponential-dist 3000) 2500)
(exponential_dist_cdf 1/3000 2500)
(newline)
(displayln "Quantile:")
(exponential_dist_quantile 1/3000 0.5654017914929218)
(dist-inv-cdf (exponential-dist 3000) 0.5654017914929218)
(newline)
