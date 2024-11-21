#| 

  Multinomial callcenter in Racket/Gamble 

  From Mathematica (MultinomialDistribution)
  """
  In calling a customer service center, one of three things may happen: the line 
  is busy with probability 0.4, a caller gets the wrong party with probability 
  0.1, or a caller gets connected to an agent. Find the probability that a caller 
  calling at 6 different times gets a busy signal 4 times and twice connects 
  directly to an agent:

  D = MultinomialDistribution(6, (0.4, 0.1, 0.5))
  PDF(D, (4, 0, 2))
  -> 0.096
  """

  variable : p
  #f: 0.9039999999999999
  #t: 0.096
  mean: 0.096

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

   (define v (multinomial_dist 6 '(4/10 1/10 5/10)))

   (define p (equal? v '(4 0 2)))
   
   (list v
         p)
   )
)

(show-marginals (model)
                (list  "v"
                       "p"
                       ))


