#| 

  Cloud duration in Racket.Gamble 

  From Mathematica (Probability)
  """
  Cloud duration approximately follows a beta distribution with parameters 0.3 and 0.4 for a particular location. 
  Find the probability that cloud duration will be longer than half a day:

  Probability(x > 0.5, x e BetaDistribution(0.3, 0.4))
  -> 0.421508

  Find the average cloudiness duration for a day:
  Mean(BetaDistribution(0.3, 0.4))
  -> 0.428571

  Find the probability of having exactly 20 days in a month with cloud duration less than 10%:
  p = Probability(x < 0.1, x e BetaDistribution(0.3, 0.4));
  -> 0.331541

  Probability(k == 20, k e BinomialDistribution(30, p))
  -> 0.000137807

  Find the probability of at least 20 days in a month with cloud duration less than 10%:
  Probability(k >= 20, k e BinomialDistribution(30, p))
  0.000178284  
  """

  var : p
  mean: 0.4286138345034924

  var : p1
  mean: 0.4225800000005614

  var : p2
  mean: 0.3313200000004701

  var : pp
  mean: 0.33500000000131136

  var : k
  mean: 10.049880000019826

  var : p3
  mean: 0.00012000000000022996

  var : p4
  mean: 0.0001700000000003258


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

;
; Calculate the mean of beta(0.3,0.4) < 0.1
;
(define pp
  (dist-pdf (sampler->discrete-dist (importance-sampler (define p (beta 0.3 0.4)) (< p 0.1)) 1000) #t))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define p (beta 0.3 0.4))
   (define p1 (> p 0.5))
   (define p2 (< p 0.1 )) ; Same as pp calculated above
   (define k (binomial 30 pp)) ; using pp
   (define p3 (= k 20))
   (define p4 (>= k 20))

   (list p
         p1
         p2
         pp
         k
          p3
         p4
         )
   )
)

(show-marginals (model)
                (list  "p"
                       "p1"
                       "p2"
                       "pp"
                       "k"
                       "p3"
                       "p4"
                       )
                #:num-samples 100000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


