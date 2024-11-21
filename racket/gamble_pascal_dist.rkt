#| 

  Pascal distribution in Racket/Gamble 

  From Handbook on probability distributions
  page 25.

  The Pascal distribution gives the distribution of the number of trials 
  with nonzero success probability p before n successes occur

  Here's a simple example from Mathematica PascalDistribution:
  """
  Find the probability of getting 3 heads in no more than 6 flips

  heads3 = PascalDistribution(3, 1/2)
  Probability(x <= 6, x e heads3)
  -> 
  0.65625
  """

  (pascal_cdf 3 0.5 6):: 0.6562499999999996

  variable : d
  5: 0.19200000000000003
  4: 0.18530000000000002
  6: 0.15700000000000003
  3: 0.12020000000000002
  7: 0.11850000000000001
  8: 0.08720000000000001
  9: 0.051800000000000006
  10: 0.033900000000000007
  11: 0.023700000000000002
  12: 0.012900000000000002
  13: 0.0068000000000000005
  14: 0.004900000000000001
  15: 0.0026000000000000003
  16: 0.0009000000000000001
  17: 0.0009000000000000001
  18: 0.0009000000000000001
  19: 0.00030000000000000003
  20: 0.00010000000000000002
  23: 0.00010000000000000002
  mean: 6.0035
  Min: 3 Mean: 6.0118 Max: 21 Variance: 6.02926076 Stddev: 2.455455306048147
  HPD interval (0.84): 3..8
  Percentiles:
  (0.01 3)
  (0.025 3)
  (0.1 3)
  (0.05 3)
  (0.25 4)
  (0.5 6)
  (0.75 7)
  (0.84 8)
  (0.9 9)
  (0.95 11)
  (0.975 12)
  (0.99 14)
  (0.999 17)
  Histogram:
 3: 1257 ###################################################### (0.125 / 0    )
 4: 1813 ############################################################################# (0.181 / 0.125)
 5: 1893 ################################################################################ (0.189 / 0.307)
 6: 1601 #################################################################### (0.160 / 0.496)
 7: 1168 ################################################## (0.116 / 0.656)
 8:  844 #################################### (0.084 / 0.773)
 9:  518 ###################### (0.051 / 0.857)
10:  356 ################ (0.035 / 0.909)
11:  208 ######### (0.020 / 0.945)
12:  134 ###### (0.013 / 0.965)
13:   82 #### (0.008 / 0.979)
14:   56 ### (0.005 / 0.987)
15:   33 ## (0.003 / 0.993)
16:   15 # (0.001 / 0.996)
17:   14 # (0.001 / 0.997)
18:    2 # (0.000 / 0.999)
19:    3 # (0.000 / 0.999)
20:    1 # (0.000 / 0.999)
21:    2 # (0.000 / 0.999)

  variable : p
  #t: 0.6545000000000001
  #f: 0.34550000000000003
  mean: 0.6545000000000001
  Min: 0 Mean: 0.6564 Max: 1 Variance: 0.22553904 Stddev: 0.4749095071695238
  Percentiles:
  (0.01 #f)
  (0.025 #f)
  (0.1 #f)
  (0.05 #f)
  (0.25 #f)
  (0.5 #t)
  (0.75 #t)
  (0.84 #t)
  (0.9 #t)
  (0.95 #t)
  (0.975 #t)
  (0.99 #t)
  (0.999 #t)
  Histogram:
  #f: 3436 ########################################## (0.343 / 0    )
  #t: 6564 ################################################################################ (0.656 / 0.343)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(show "(pascal_cdf 3 0.5 6):" (pascal_cdf 3 0.5 6))
(newline)

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; Cf negative_binomial_test.wppl
   (define d (pascal_dist 3 0.5))
   (define p (<= d 6))
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
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                #:show-histogram? #t
                #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


