#| 

  Gamma dist in Racket/Gamble 

  From Handbook on probability distributions
  page 60
  """
  Simulate a gamma G(a, lambda) is quite tricky for non integer shape parameter.
  Indeed, if the shape parameter a is integer, then we simply sum a exponential
  random variables E(lambda). Otherwise we need to add a gamma variable
  G(α−abs(α), lambda). This is carried out by an acceptance/rejection method.
  """

  See gamble_distributions.rkt and gamble_distributions_test.rkt for more on this.

  Example from 
  https://medium.com/pythons-gurus/understanding-the-basics-of-gamma-distribution-24bfd9bee253
  """
  Suppose we want to model the time until the next earthquake in a region 
  that has an average of 2 earthquakes per year. If we assume the time between 
  earthquakes follows a Gamma Distribution, we can use the shape parameter k 
  (number of events) and the scale parameter θ (average time between events) to 
  understand the distribution of waiting times.
  
  ...

  Suppose a company manufactures a component, and the time to produce a component 
  follows a Gamma distribution with a shape parameter k=3 and a rate parameter 
  λ=2(which means the mean production time is k/λ = 3/2 ​= 1.5 hours). 
  We want to find the probability that the production time for a component is 
  less than 2 hours.

  --- 

  The Gamma Distribution is widely used in various fields, including:

  - Engineering: To model the time until failure of systems or components.
  - Finance: To assess the time between market events or transactions.
  - Healthcare: To study the duration of patient recovery times or the 
    time between disease occurrences.

  Connection with Poisson Distribution

  It’s also worth noting that the Gamma Distribution is related to the Poisson 
  Distribution. While the Poisson Distribution models the number of events occurring 
  within a fixed interval, the Gamma Distribution models the time until 
  the k-th event occurs. 
  """

  Note: For this example:
   k=a
   λ=b (rather the rate: 1/b)

  
  variable : d1
  mean: 6.035233477700917
  HPD interval (0.84): 1.0046970887864175..9.525340222254574
  HPD interval (0.99): 0.3902437590217982..17.19451137650888

  variable : d1_rate
  mean: 1.4928200777797476
  HPD interval (0.84): 0.2970757247731093..2.4443257313707294
  HPD interval (0.99): 0.07691711452438109..4.175717383047386

  variable : d2
  mean: 6.03771217673828
  HPD interval (0.84): 1.0576199776636974..9.597238958898366
  HPD interval (0.99): 0.13059868549638232..16.52049531305896

  variable : d2_rate
  mean: 1.5123920389148833
  HPD interval (0.84): 0.29742986669107163..2.3953964528240173
  HPD interval (0.99): 0.04867055664702824..4.1960162156533976

  variable : mean
  mean: 5.999999999999678
  HPD interval (0.84): 6..6
  HPD interval (0.99): 6..6
 
  variable : mean_rate
  mean: 1.4999999999999194
  HPD interval (0.84): 3/2..3/2
  HPD interval (0.99): 3/2..3/2


  (a 3 b (shape) 2)
  (gamma_dist_pdf a b 24): 0.09196986029286058
  (gamma_dist_cdf a b 24): 0.0803013970713942
  (gamma_dist_quantile_est a b 0.5): 5.307394160690941
  (gamma_dist_mean a b): 6
  (dist-mean gamma a b): 6.0

  (k (a) 3 λ (rate, 1/b)) 1/2)
  (gamma_dist_pdf a b_rate 24): 0.29305022221974686
  (gamma_dist_cdf a b-rate 24): 0.7618966944464556
  (gamma_dist_quantile_est a b_rate 0.5): 1.2880349605569341
  (gamma_dist_mean a b_rate): 3/2
  (dist-mean gamma a b_rate): 1.5



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

   (define a 3) ; shape
   (define b 2) ; scale
   (define b_rate (/ 1 b)) ; rate=1/scale   
    
   (define d1 (gamma_dist a b))
   (define d1_rate (gamma_dist a b_rate))   
   (define d2 (gamma a b)) ; built-in method
   (define d2_rate (gamma a b_rate)) ; built-in method   
       
   (list d1
         d1_rate
         d2
         d2_rate
         (gamma_dist_mean a b)
         (gamma_dist_mean a b_rate)         
         )

   )
)

(show-marginals (model)
                (list  "d1"
                       "d1_rate"
                       "d2"
                       "d2_rate"
                       "mean"
                       "mean_rate"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                #:hpd-interval (list 0.84 0.99)
                )


(newline)
(let* ([a 3]
       [b 2]
       [b_rate (/ 1 b)])
  (show2 "a" a "b (shape)" b)
  (show "(gamma_dist_pdf a b 24)" (gamma_dist_pdf a b 2))
  (show "(gamma_dist_cdf a b 24)" (gamma_dist_cdf a b 2))
  (show "(gamma_dist_quantile_est a b 0.5)" (gamma_dist_quantile_est a b 0.5))
  (show "(gamma_dist_mean a b)" (gamma_dist_mean a b))
  (show "(dist-mean gamma a b)" (dist-mean (gamma-dist a b)))
  (newline)
  (show2 "k (a)" a "λ (rate, 1/b))" b_rate)
  (show "(gamma_dist_pdf a b_rate 24)" (gamma_dist_pdf a b_rate 2))
  (show "(gamma_dist_cdf a b-rate 24)" (gamma_dist_cdf a b_rate 2))
  (show "(gamma_dist_quantile_est a b_rate 0.5)" (gamma_dist_quantile_est a b_rate 0.5))
  (show "(gamma_dist_mean a b_rate)" (gamma_dist_mean a b_rate))
  (show "(dist-mean gamma a b_rate)" (dist-mean (gamma-dist a b_rate)))
  (newline)
)
