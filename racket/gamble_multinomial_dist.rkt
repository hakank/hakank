#| 

  Multinomial dist in Racket.Gamble 

  Here's a definition of the multinomial distribution that WebPPL etc use,
  i.e. that the values sums to 1. The existing multinomial-dist 
  "representing counts of n iterated samples from the corresponding categorical 
  distribution with weights for weights.", which is not what I want.

  Example from Mathematica MultinomialDistribution:
  """
  There are two candidates in an election where the winner is chosen 
  based on a simple majority. Each of n voters votes for candidate 1 with 
  probability p1 and for candidate 2 with probability p2, 
   p1 + p2 < 1,  
  so that a voter may choose not to vote for either candidate. When 
  n=100, p1 =p2 =0.4, the probability of one swing vote is: 
   D =  MultinomialDistribution[100 - 1, {0.4, 0.4, 1 - 0.4 - 0.4}]
   Probability[x == y, {x, y, z} e D]
   -> 0.0447288

  Probability that a winner won by one vote:
  Probability[ Abs[x - y] == 1, {x, y, z} e D]
  -> 0.0888996

  Probability that candidate 1 wins the election:
  Probability[x > y, {x, y, z} e D]
  -> 0.477636

  Probable outcome of the next election:
  RandomVariate[D]
  -> {52, 36, 11}

  Average outcome of an election:
  Mean[D] 
  -> {39.6, 39.6, 19.8}
  """

  And: probability that z > x && z > y
  Probability[{z > x && z > y}, {x, y, z} e D]
  -> 0.000225849


  variable : m
  (40 39 20): 0.008830557464742105
  (39 40 20): 0.008830557464742105
  (40 40 19): 0.008830557464742105
  (39 41 19): 0.008615178014382543
  (41 39 19): 0.008615178014382543
  ...
  (0 2 97): 1.2298746123413814e-65
  (2 0 97): 1.2298746123413814e-65
  (0 1 98): 1.2549740942259455e-67
  (1 0 98): 1.2549740942258742e-67
  (0 0 99): 6.338253001141011e-70
  
  variable : x
  40: 0.08121914499610601
  39: 0.081219144996106
  41: 0.07791755373610171
  38: 0.07789049151265902
  42: 0.07173362089990315
  ...
  95: 7.65591624360844e-33
  96: 2.126643401002301e-34
  97: 4.3848317546438986e-36
  98: 5.965757489311648e-38
  99: 4.017345110647503e-40
  mean: 39.599999999999945

  variable : y
  39: 0.081219144996106
  40: 0.081219144996106
  41: 0.07791755373610168
  38: 0.077890491512659
  42: 0.07173362089990314
  ...
  95: 7.655916243608396e-33
  96: 2.1266434010023294e-34
  97: 4.384831754643989e-36
  98: 5.965757489311478e-38
  99: 4.017345110647503e-40
  mean: 39.59999999999995

  variable : z
  19: 0.09930021480882467
  20: 0.0993002148088246
  21: 0.09338948773687074
  18: 0.09317057191939106
  22: 0.08277704594858999
  ...
  95: 6.108049274732346e-61
  96: 6.36255132784614e-63
  97: 4.91949844936553e-65
  98: 2.50994818845182e-67
  99: 6.338253001141011e-70
  mean: 19.799999999999986

  variable : p1
  #f: 0.9552711890260176
  #t: 0.044728810973981147
  mean: 0.044728810973981147

  variable : p2
  #f: 0.9111004319924804
  #t: 0.08889956800751814
  mean: 0.08889956800751814

  variable : p3
  #f: 0.5223644054869903
  #t: 0.47763559451300863
  mean: 0.47763559451300863

  variable : p4
  #f: 0.999774150518559
  #t: 0.00022584948144001757
  mean: 0.00022584948144001757


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define m (multinomial_dist 99 (list 4/10 4/10 2/10)))

   (define x (first m))
   (define y (second m)) 
   (define z (third m))  

   (define p1 (= x y))               ; Mathematica: 0.0447288
   (define p2 (= 1 (abs (- x y))))   ; Mathematica: 0.0888996
   (define p3 (> x y))               ; Mathematica: 0.477636
   (define p4 (and (> z x) (> z y))) ; Mathematica: 0.000225849
   
   (list m
         x
         y
         z
         p1
         p2
         p3
         p4
         )

   )
)

(show-marginals (model)
                (list  "m"
                       "x"
                       "y"
                       "z"
                       "p1"
                       "p2"
                       "p3"
                       "p4"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


