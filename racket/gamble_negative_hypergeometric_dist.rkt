#| 

  Negative Hypergeometric distribution in Racket/Gamble 

  From Mathematica BetaBinomialDistribution
  """
  Define a negative hypergeometric distribution:
  NegativeHypergeometricDistribution(w_, wtot_, btot_) := BetaBinomialDistribution(w, wtot - w + 1, btot)
  Find the probability that b black balls were sampled without replacement before a w^th 
  white ball was drawn from an urn initially filled with k_b black and  k_w white balls:

  Mean@NegativeHypergeometricDistribution(3, 10, 20) ;; N
  -> 5.45455
  Probability(x > 10, x ~ NegativeHypergeometricDistribution(3, 10, 20)) ;; N
  -> 0.0742771
  """

  variable : d
  4: 0.12117
  5: 0.11821
  3: 0.117
  6: 0.10811
  2: 0.09673
  ...
  15: 0.00347
  16: 0.00176
  17: 0.00063
  18: 0.00025
  19: 4e-5
  mean: 5.4608300000000005
  Histogram:
   0:  2988 #################### (0.029 / 0    )
   1:  6551 ########################################### (0.065 / 0.029)
   2:  9777 ################################################################# (0.097 / 0.095)
   3: 11459 ############################################################################ (0.114 / 0.193)
   4: 12220 ################################################################################ (0.122 / 0.307)
   5: 11963 ############################################################################### (0.119 / 0.429)
   6: 10915 ######################################################################## (0.109 / 0.549)
   7:  9147 ############################################################ (0.091 / 0.658)
   8:  7556 ################################################## (0.075 / 0.750)
   9:  5805 ####################################### (0.058 / 0.825)
  10:  4359 ############################# (0.043 / 0.883)
  11:  2811 ################### (0.028 / 0.927)
  12:  1994 ############## (0.019 / 0.955)
  13:  1141 ######## (0.011 / 0.975)
  14:   667 ##### (0.006 / 0.986)
  15:   373 ### (0.003 / 0.993)
  16:   181 ## (0.001 / 0.997)
  17:    61 # (0.000 / 0.999)
  18:    22 # (0.000 / 0.999)
  19:     6 # (6e-5  / 0.999)
  20:     4 # (4e-5  / 0.999)


  variable : p
  #f: 0.92618
  #t: 0.07382
  mean: 0.07382

  (negative_hypergeometric_pdf w wtot btot 10)) : 0.042721496394659816
  (- 1 (negative_hypergeometric_cdf w wtot btot 10)): 0.07427714714071532
  quantile 0.9: 10
  mean: 5.454545454545454

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

   (define w 3)
   (define wtot 10)
   (define btot 20)

   (define d (negative_hypergeometric w wtot btot))
   (define p (> d 10))

   (list d
         p
         )
    
   )
)

(show-marginals (model)
                (list  "d"
                       "p"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


(let ([w 3]
      [wtot 10]
      [btot 20])
  (show "(negative_hypergeometric_pdf w wtot btot 10)) " (* 1.0 (negative_hypergeometric_pdf w wtot btot 10)))
  (show "(- 1 (negative_hypergeometric_cdf w wtot btot 10))" (- 1.0 (negative_hypergeometric_cdf w wtot btot 10)))
  (show "quantile 0.9" (negative_hypergeometric_quantile w wtot btot 0.9))
  (show "mean" (* 1.0 (negative_hypergeometric_mean w wtot btot)))
)

(newline)
