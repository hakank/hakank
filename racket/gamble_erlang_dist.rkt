#| 

  Erlang dist in Racket/Gamble 

  See gamble_distributions.rkt and gamble_distributions_test.rkt
  for more on this.

  From Mathematica's ErlangDistribution:
  """
  Assume that the delay caused by a traffic signal is exponentially distributed 
  with an average delay of 0.5 minutes. A driver has to drive a route that passes through 
  seven unsynchronized traffic signals. Find the distribution for the delay passing all signals:

  Mean[ExponentialDistribution[Lambda]] == 0.5
  -> 1/Lambda == 0.5

  Hence the distribution for the sum of 7 independent exponential variables:
  trafficDelayDistribution = ErlangDistribution[7, 2]

  Find the probability that traffic signals cause a delay greater than 5 minutes
  Probability[d > 5, d e trafficDelayDistribution]
  -> 25799/(9 E^10)
  -> 0.130141
  """  


  var : d1
  mean: 3.499579143614041
  Min: 0.46557501400223333 Mean: 3.5196761565431247 Max: 11.103960953238245 Variance: 1.8319928209784302 Stddev: 1.3535112932585491
  Credible interval (0.84): 1.5357365633311477..5.112338085984371

  var : d2
  mean: 3.506127524077066
  Min: 0.6394217998792929 Mean: 3.50302173995633 Max: 10.492639595538794 Variance: 1.6914842164003647 Stddev: 1.300570727181096
  Credible interval (0.84): 1.6373777398967078..5.149397931339985

  var : p1
  mean: 0.13020000000000878
  Min: 0 Mean: 0.1376 Max: 1 Variance: 0.11866624 Stddev: 0.34447966558274523
  Credible interval (0.84): 0..0

  var : p2
  mean: 0.1303000000000088
  Min: 0 Mean: 0.1296 Max: 1 Variance: 0.11280384 Stddev: 0.3358628291430893
  Credible interval (0.84): 0..0

  And just a test of the PDF and mean:

  (erlang_pdf 7 2 5)
  0.12611091600690236

  (erlang_cdf 7 2 5)
  0.8698585791175171

  (erlang_mean 7 2)
  7/2
  3.5


  
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

   (define d1 (erlang 7 2)) ; Mathematica's version
   (define lambdas (ones-list 7 2))  ; Sum of over (perhaps different) lambdas 
   (define d2 (erlang2 lambdas))

   (define p1 (> d1 5))
   (define p2 (> d2 5))
   
   (list d1
         d2
         p1
         p2
         )
   
   )
  )

(show-marginals (model)
                (list  "d1"
                       "d2"
                       "p1"
                       "p2"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(displayln "(erlang_pdf 7 2 5)")
(erlang_pdf 7 2 5)
(newline)
(displayln "(erlang_cdf 7 2 5)")
(erlang_cdf 7 2 5)
(newline)
(displayln "(erlang_mean 7 2)")
(erlang_mean 7 2)
(erlang_mean 7 2.0)
(newline)

