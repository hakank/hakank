#| 

  Lottery in Racket/Gamble 

  From Mathematica (BernoulliDistribution)
  """
  A lottery sells 10 tickets for $1 per ticket. Each time there 
  is only one winning ticket. A gambler has $5 to spend. Find his probability of 
  winning if he buys 5 tickets in 5 different lotteries:

  D = ProductDistribution((BernoulliDistribution(1/10), 5));
  Probability(1 <= x + y + u + v + w, (x, y, u, v, w) -> D)
  ->
  40951/100000 (0.40951)

  His probability of winning is greater if he buys 5 tickets in the same lottery:
  PDF(HypergeometricDistribution(5, 1, 10), 1)
  -> 1/2

  """

  variable : p_different_lotteries
  #f: 59049/100000 (0.59049)
  #t: 40951/100000 (0.40951)
  mean: 40951/100000 (0.40951)

  variable : p_same_lottery
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  variable : p_same_lottery2
  1/2: 1 (1.0)
  mean: 1/2 (0.5)

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

   (define n 5)
   (define win (for/list ([i n]) (bernoulli 1/10)))

   (define p_different_lotteries (<= 1 (sum win)))
   (define p_same_lottery (hypergeometric 1 10 1 5))
   ; Same parameter order as Mathematica
   (define p_same_lottery2 (hypergeometric2_pdf 5 1 10 1))   
    
   (list p_different_lotteries
         p_same_lottery
         p_same_lottery2
   )
)

(show-marginals (model)
                (list  "p_different_lotteries"
                       "p_same_lottery"
                       "p_same_lottery2"
                       ))


