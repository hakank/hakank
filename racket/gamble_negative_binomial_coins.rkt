#| 

  Negative binomial coins in Racket/Gamble 

  From Mathematica (NegativeBinomialDistribution)  
  """
  The number of tails before getting 4 heads with a fair coin:
    heads4 = NegativeBinomialDistribution(4, 1/2)

  Compute the probability of getting at least 6 tails before getting 4 heads:
    Probability(tails >= 6, tails -> heads4)
    -> 
    65/256  (0.25390625)


  Compute the expected number of tails before getting 4 heads:
     Mean(heads4)
     -> 
     4
  """

  "Almost exact" with enumerate #:limit 1-e-10

  variable : heads4
  2: 0.1562500000155766
  3: 0.15625000001557646
  4: 0.13671875001362954
  1: 0.12500000001246117
  5: 0.10937500001090356
  ...
  40: 7.015046322312108e-10
  41: 3.7641711973380007e-10
  42: 2.0165202842880824e-10
  43: 1.0786038729912277e-10
  44: 2.3547386265437424e-11
  mean: 3.9999999958724564

  variable : p_at_least_six_tails
  #f: 0.7460937500743778
  #t: 0.25390624992562233
  mean: 0.25390624992562233

  Exact results:
  (negative_binomial_mean 4 1/2):: 4
  ((- 1 (negative_binomial_cdf 4 1/2 5): 65/256 0.25390625)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-10
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define heads4 (negative_binomial 4 1/2))
   (define p_at_least_six_tails (>= heads4 6))

   (list heads4
         p_at_least_six_tails
         )
   )
)

(show-marginals (model)
                (list  "heads4"
                       "p_at_least_six_tails"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

(newline)
(let* ([m 4]
       [p 1/2]
       [v (- 1 (negative_binomial_cdf m p 5))])
  (show "(negative_binomial_mean 4 1/2):" (negative_binomial_mean m p))
  (show2 "(- 1 (negative_binomial_cdf 4 1/2 5):" v (* 1.0 v) )
  )
