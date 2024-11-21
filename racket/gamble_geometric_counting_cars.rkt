#| 

  Geometric counting cards in Racket/Gamble 

  From Mathematica (GeometricDistribution) 
  """
  A person is standing by a road counting cars until he sees a red one, 
  at which point he restarts the count. Simulate the counting process, assuming 
  that 20% of the cars are red:

    RandomVariate(GeometricDistribution(0.2), 20)
    -> 
     (10,0,2,0,2,10,8,4,0,0,15,4,4,1,6,1,6,1,0,0)

  Find the expected number of cars to come by before the count starts over:
    Mean(GeometricDistribution(0.2))
    -> 4

  Find the probability of counting 10 or more cars before a red one:
    NProbability(x >= 10, x -> GeometricDistribution(0.2))
    -> 
    0.107374

  """

  var : v
  0: 0.20000000000000004
  1: 0.16000000000000003
  2: 0.128
  3: 0.10240000000000002
  4: 0.08192
  5: 0.06553600000000001
  6: 0.052428800000000005
  7: 0.04194304000000001
  8: 0.03355443200000001
  9: 0.02684354560000002
  ...
  151: 4.65176783549188e-16
  152: 3.7214142683935074e-16
  153: 2.9771314147148087e-16
  154: 2.381705131771849e-16
  155: 1.905364105417468e-16
  156: 1.5242912843339756e-16
  157: 1.2194330274671816e-16
  mean: 3.9999999999999223
  HPD interval (0.84): 0..7
  Percentiles:
  (0.01 0)
  (0.025 0)
  (0.1 0)
  (0.05 0)
  (0.25 1)
  (0.5 2)
  (0.75 5)
  (0.84 7)
  (0.9 9)
  (0.95 12)
  (0.975 15)
  (0.99 20)
  (0.999 29)

  var : p_at_least_10
  #f: 0.8926258176000003
  #t: 0.10737418239999962
  mean: 0.10737418239999962

  Using dist-cdf: 0.10737418239999996 

  Samples
  '(10 6 12 0 4 1 5 0 1 0 8 5 0 6 1 0 8 15 22 8)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-25

   (define v (geometric 2/10))
   (define p_at_least_10 (>= v 10))

   (list v
         p_at_least_10)
   )
)

(show-marginals (model)
                (list  "v"
                       "p_at_least_10"
                       )
                #:hpd-interval (list 0.84)
                #:show-percentiles? #t
                )


(displayln (format "Using dist-cdf: ~a " (- 1 (dist-cdf (geometric-dist 2/10) 9))))

(displayln "Samples")
(repeat (lambda () (geometric 2/10)) 20)
