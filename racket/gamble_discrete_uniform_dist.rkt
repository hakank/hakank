#| 

  Discrete uniform dist in Racket/Gamble 

  From Mathematica DiscreteUniformDistribution

  variable : d
  8: 1/6 (0.16666666666666666)
  9: 1/6 (0.16666666666666666)
  10: 1/6 (0.16666666666666666)
  5: 1/6 (0.16666666666666666)
  6: 1/6 (0.16666666666666666)
  7: 1/6 (0.16666666666666666)
  mean: 15/2 (7.5)

  (discrete_uniform_dist_mean 5 10): 15/2
  (discrete_uniform_dist_pdf 5 10 8): 1/6
  (discrete_uniform_dist_cdf 5 10 8 4)
  (discrete_uniform_dist_cdf 5 10 8): 2/3
  (discrete_uniform_dist_quantile 5 10 0.99): 10.0

  See gamble_distributions.rkt and gamble_distributions_test.rkt for more on this.

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
   (define a 5)
   (define b 10)
   (define d (discrete_uniform_dist a b))
   (list d)
   
   )
)

(show-marginals (model)
              (list  "d"))


(show "(discrete_uniform_dist_mean 5 10)" (discrete_uniform_dist_mean 5 10))
(show "(discrete_uniform_dist_pdf 5 10 8)" (discrete_uniform_dist_pdf 5 10 8))
(show "(discrete_uniform_dist_cdf 5 10 8)" (discrete_uniform_dist_cdf 5 10 8))
(show "(discrete_uniform_dist_quantile 5 10 0.99)" (discrete_uniform_dist_quantile 5 10 0.99))


(newline)


#|
  From Mathematica DiscreteUniformDistribution
  """
  A fair six-sided die can be modeled using a DiscreteUniformDistribution: 

  dice = DiscreteUniformDistribution[{1, 6}]

  Generate 10 throws of the die:
  RandomVariate[dice, 10]
  -> {5, 2, 1, 5, 3, 6, 1, 5, 5, 2}

  Compute the probability that the sum of three dice values is less than 6:
  Probability[ x1 + x2 + x3 <= 6, {x1 e dice, x2 e dice, x3 e dice}]
  -> 5/54
  -> 0.0925926
  """

  10 random values: (4 4 1 6 1 1 6 3 5 1)

  variable : s
  10: 1/8 (0.125)
  11: 1/8 (0.125)
  9: 25/216 (0.11574074074074074)
  12: 25/216 (0.11574074074074074)
  8: 7/72 (0.09722222222222222)
  13: 7/72 (0.09722222222222222)
  7: 5/72 (0.06944444444444445)
  14: 5/72 (0.06944444444444445)
  6: 5/108 (0.046296296296296294)
  15: 5/108 (0.046296296296296294)
  16: 1/36 (0.027777777777777776)
  5: 1/36 (0.027777777777777776)
  17: 1/72 (0.013888888888888888)
  4: 1/72 (0.013888888888888888)
  18: 1/216 (0.004629629629629629)
  3: 1/216 (0.004629629629629629)
  mean: 21/2 (10.5)

  variable : p
  #f: 49/54 (0.9074074074074074)
  #t: 5/54 (0.09259259259259259)
  mean: 5/54 (0.09259259259259259)


|#

(show "10 random values" (repeat (lambda () (discrete_uniform_dist 1 6)) 10))
(newline)
(define (model2)
  (enumerate

   (define n 3) ; number of dice
   (define ds (for/list ([i n]) (discrete_uniform_dist 1 6)))
   (define s (sum ds))
   (define p (<= s 6))

   (list s p)
   
   )
)

(show-marginals (model2)
                (list  "s" "p"))

