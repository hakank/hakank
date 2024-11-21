#| 

  Geometric coin in Racket/Gamble 

  From Mathematica (GeometricDistribution) 
  """
  A coin-tossing experiment consists of tossing a fair coin repeatedly 
  until a tail results. Simulate the process:
    RandomVariate(GeometricDistribution(1/2), 10)
    -> 
     (0,0,1,1,2,0,0,2,0,1)

  Compute the probability that at least 4 coin tosses will be necessary:
    Probability(x >= 4, x -> GeometricDistribution(1/2))
    -> 1/16 (0.0625)
    
  Compute the expected number of coin tosses:
    Mean(GeometricDistribution(1/2)) 
    -> 1

  """

  Using enumerate #:limit 1e-15
  var : v
  0: 0.5000000000000004
  1: 0.2500000000000002
  2: 0.12500000000000014
  3: 0.06250000000000007
  4: 0.03125000000000003
  ...
  45: 1.4210854715202035e-14
  46: 7.10542735760098e-15
  47: 3.5527136788004958e-15
  48: 1.776356839400257e-15
  49: 8.88178419700127e-16
  mean: 0.9999999999999557

  var : p_at_least_4
  #f: 0.9375000000000009
  #t: 0.062499999999999174
  mean: 0.062499999999999174

  Using dist-cdf: 0.0625 

  Samples
  '(0 3 1 1 2 0 4 1 1 0 1 0 1 0 1 0 0 0 2 0)

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

   (define v (geometric 1/2))
   (define p_at_least_4 (>= v 4))

   (list v
         p_at_least_4)
    
   )
)

(show-marginals (model)
                (list  "v"
                       "p_at_least_4"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(displayln (format "Using dist-cdf: ~a " (- 1 (dist-cdf (geometric-dist 1/2) 3))))

(displayln "Samples")
(repeat (lambda () (geometric 1/2)) 20)
