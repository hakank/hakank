#| 

  Multinomial balls in Racket/Gamble 

  From Mathematica (Multinomial)
  """
  Distribute 5 balls among 3 containers, picking each container with equal probability. 
  Find the probability that no container is empty:

    Probability(x) > 0 && x2 > 0 && x3 > 0, (x1, x2, x3) ->) MultinomialDistribution(5, (1/3, 1/3, 1/3)))
    -> 
    50/81  = 0.61728395061728395062
  """

  Note: Gamble's built-in multinomial does not work like Mathmatica's (or WebPPL's) 
  multinomial, so I use my own multinomial_dist.

  variable : p_no_empty
  #t: 0.6172839506172839
  #f: 0.38271604938271614
  mean: 0.6172839506172839

  variable : balls
  (2 1 2): 0.12345679012345682
  (2 2 1): 0.12345679012345682
  (1 2 2): 0.12345679012345677
  (1 1 3): 0.0823045267489712
  (1 3 1): 0.08230452674897115
  (3 1 1): 0.08230452674897115
  (2 0 3): 0.04115226337448563
  (0 2 3): 0.04115226337448561
  (3 0 2): 0.04115226337448561
  (0 3 2): 0.04115226337448561
  (2 3 0): 0.04115226337448559
  (3 2 0): 0.041152263374485576
  (0 1 4): 0.020576131687242805
  (1 0 4): 0.020576131687242805
  (4 0 1): 0.020576131687242805
  (4 1 0): 0.020576131687242795
  (0 4 1): 0.020576131687242795
  (1 4 0): 0.020576131687242784
  (0 0 5): 0.004115226337448564
  (0 5 0): 0.0041152263374485566
  (5 0 0): 0.0041152263374485566


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

   (define ps '(1/3 1/3 1/3))

   (define n 5)
   
   (define balls (multinomial_dist 5 ps))    
   
   (define b1 (list-ref balls 0))
   (define b2 (list-ref balls 1))
   (define b3 (list-ref balls 2))

   ; Prob that no container is empty
   (define p_no_empty (and (> b1 0) (> b2 0) (> b3 0)))
    
   (list p_no_empty
         balls
    )

   )
)

(show-marginals (model)
                (list  "p_no_empty"
                       "balls"
                       ))


