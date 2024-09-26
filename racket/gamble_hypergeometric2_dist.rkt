#| 

  Hypergeometric2 distribution in Racket.Gamble 

  hypergeometric2 mirrors Mathematica's version of Hypergeometric[n, n_succ, n_tot]

  """
  A hypergeometric distribution gives the distribution of the number of successes in 
  n draws from a population of size n_tot containing n_succ successes.
  """

  This is a port of my WebPPL model hypergeometric2_dist.wppl 

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


#|

  Suppose an urn has 100 elements, of which 40 are special and we draw 10 elements.
  What is the probability of getting 0..10 special elements?
  What is the probability that we get >= 7 special elements?

  Mathematica
  Table((n, PDF(HypergeometricDistribution(10, 40, 100), n)), (n, 1, 10)) 
  -> 
  ((0., 0.00435544),
   (1., 0.0341603), 
   (2., 0.115291), 
   (3., 0.220431), 
   (4., 0.264313), 
   (5., 0.207606), 
   (6., 0.108128), 
   (7., 0.0368556), 
   (8., 0.0078636), 
   (9., 0.000947778), 
   (10., 0.0000489685))

   Probability(x >= 7, x ~ HypergeometricDistribution(10, 40, 100)) ;; N
   -> 0.04571

  This Gamble model using hypergeometric2 10 40 100):

var : h
4: 6014165/22753969 (0.2643127886831524)
3: 35109720/159277783 (0.22043074268556337)
5: 259811928/1251468295 (0.20760568129294876)
2: 36726615/318555566 (0.11529107923356768)
6: 54127485/500587318 (0.10812795900674416)
7: 64573140/1752055613 (0.03685564517523109)
1: 5440980/159277783 (0.03416031977290895)
8: 5009985/637111132 (0.00786359670764629)
0: 13874499/3185555660 (0.004355440771045891)
9: 150960/159277783 (0.0009477781342549199)
10: 1258/25689965 (4.896853693650419e-5)
mean: 4 (4.0)
Min: 0 Mean: 3.936 Max: 8 Variance: 2.069904 Stddev: 1.4387160943007484
Credible interval (0.84): 1..5
Percentiles::
(0.01 1)
(0.1 2)
(0.025 1)
(0.25 3)
(0.5 4)
(0.75 5)
(0.84 5)
(0.9 6)
(0.975 7)
(0.99 7)
(0.999 8)
Histogram:
0: 2  
1: 31 
2: 140
3: 222
4: 270
5: 197
6: 107
7: 32 
8: 8  

var : p
#f: 4777024739/5005873180 (0.9542840114459312)
#t: 228848441/5005873180 (0.045715988554068804)
mean: 228848441/5005873180 (0.045715988554068804)
Min: 0 Mean: 0.038 Max: 1 Variance: 0.036556 Stddev: 0.19119623427254
Credible interval (0.84): 0..0
Percentiles::
(0.01 #f)
(0.1 #f)
(0.025 #f)
(0.25 #f)
(0.5 #f)
(0.75 #f)
(0.84 #f)
(0.9 #f)
(0.975 #t)
(0.99 #t)
(0.999 #t)
Histogram:
#f: 963
#t: 39 


|#
(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   
   
    (define h (hypergeometric2 10 40 100))
    (define p (>= h 7))
    
    (list h
          p
          )
    
   )
)

(show-marginals (model)
                (list  "h"
                       "p"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.84
                #:show-histogram? #t
                #:show-percentiles? #t
                )


