#| 

  Indistinguishable dice in Racket/Gamble 

  From MIT 6.262 Discrete Stochastic Processes
  2. More Review; The Bernoulli Process
  https://www.youtube.com/watch?v=d4xfax4_Iww&list=PLEEF5322B331C1B98&index=2

  """
  Trivial example: Roll a white die and a red die.
  There are 36 sample outcomes, (i, j), 1 ≤ i, j ≤ 6,
  taken as equiprobable by symmetry.
  
  Roll 2 indistinguishable white dice. The white and
  red outcomes (i, j) and (j, i) for i =
  j are now indistinguishable. There are now 21 ‘ﬁnest grain’
  outcomes, but no sane person would use these as
  sample points.
  """

  So we have two indistinguishable dice where (i,j) = (j,i). 
  Which we represents as a condition that i<=j.
  There are 21 possible outcomes.
  What are the probabilities of the sums?

  Indistinguishable dice:
  var : d1 + d2
  6: 1/7 (0.14285714285714285)
  7: 1/7 (0.14285714285714285)
  8: 1/7 (0.14285714285714285)
  4: 2/21 (0.09523809523809523)
  5: 2/21 (0.09523809523809523)
  9: 2/21 (0.09523809523809523)
  10: 2/21 (0.09523809523809523)
  2: 1/21 (0.047619047619047616)
  3: 1/21 (0.047619047619047616)
  11: 1/21 (0.047619047619047616)
  12: 1/21 (0.047619047619047616)
  mean: 7 (7.0)

  Distinguishable (normal) dice:
  var : d1 + d2
  7: 1/6 (0.16666666666666666)
  6: 5/36 (0.1388888888888889)
  8: 5/36 (0.1388888888888889)
  5: 1/9 (0.1111111111111111)
  9: 1/9 (0.1111111111111111)
  4: 1/12 (0.08333333333333333)
  10: 1/12 (0.08333333333333333)
  3: 1/18 (0.05555555555555555)
  11: 1/18 (0.05555555555555555)
  2: 1/36 (0.027777777777777776)
  12: 1/36 (0.027777777777777776)
  mean: 7 (7.0)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model obs)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define d1 (add1 (random-integer 6)))
   (define d2 (add1 (random-integer 6)))

   (when obs (observe/fail (<= d1 d2)))

   (list (+ d1 d2))
   
   )
)

(displayln "Indistinguishable dice:")
(show-marginals (model #t) (list  "d1 + d2" ))
(displayln "\nDistinguishable (normal) dice:")
(show-marginals (model #f) (list  "d1 + d2" ))
