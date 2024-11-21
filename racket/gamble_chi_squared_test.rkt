#| 

  Chi squared test in Racket/Gamble 

  From Mathematica ChiSquareDistribution
  """
  The weight in grams of a particular boxed cereal product is known to 
  follow a normal distribution. A quality assurance team samples 15 boxes at 
  random and records their weights. Test the hypothesis that the standard 
  deviation of the product weight is less than 36:

  weights = 
  Quantity[{367.9, 384.7, 353.8, 334.7, 450.9, 390.6, 422.6, 352.2, 
    330.9, 342.0, 388.9, 386.7, 374.2, 388.5, 382.2}, "Grams"];

  {Mean[weights], StandardDeviation[weights]}
  -> {Quantity[376.72, "Grams"], Quantity[32.2513, "Grams"]}

  Under the null hypothesis of \[Sigma]^2>=36^2, the following statistic 
  follows ChiSquareDistribution
 
  Sigma0 = Quantity[36, "Grams"];
  Chi2stat = (15 - 1) Variance[weights]/Sigma0^2
  ->11.2362

  The null hypothesis cannot be rejected at the 5% level:
  InverseCDF[ChiSquareDistribution[14], 0.05]
  -> 6.57063

  Chi2stat < InverseCDF[ChiSquareDistribution[14], 0.05]
  -> False
  """

  Note: The built-in variance and stddev are population variance/stddev.
  Mathematica uses sample variance/stddev (which are ./(n-1) instead of ./n))

  (len: 15 sum 5650.799999999999)
  (mean 376.71999999999997)
  (stddev 31.15773205268103 32.2513166951588)    ; compares  sample vs population
  (variance 970.8042666666668 1040.1474285714287) ; compares  sample vs population
  sigma-theta: 36
  chi2-stat: 11.236160493827162
  quantile 0.05: 6.570631383791374
  null hypothesis tested: #f


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

      
(define weights '(367.9 384.7 353.8 334.7 450.9 390.6 422.6 352.2 
                        330.9 342.0 388.9 386.7 374.2 388.5 382.2))

(show2 "len:" (length weights) "sum" (sum weights))
(show2 "mean" (avg weights))
; Compares sample vs population
(show2"stddev" (stddev weights) (sample-stddev weights))
(show2 "variance" (variance weights) (sample-variance weights))

(define sigma-theta 36)
(define chi2-stat (/ (* (- (length weights) 1) (sample-variance weights)) (expt sigma-theta 2)))

(show "sigma-theta" sigma-theta)
(show "chi2-stat" chi2-stat)

(show "quantile 0.05" (chi_squared_dist_quantile (- (length weights) 1) 0.05))

(show "null hypothesis tested" (< chi2-stat (chi_squared_dist_quantile (- (length weights) 1) 0.05) ))
