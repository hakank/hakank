#| 

  Multinomial voting in Racket/Gamble 

  From Mathematica MultinomialDistribution
  """
  There are two candidates in an election where the winner is chosen based on a 
  simple majority. 
  Each of n voters votes for candidate 1 with probability p1 and for candidate 2 
  with probability p2, where p1+p2<1, so that a voter may choose not to vote for either 
  candidate. When n=100, p1=p2=0.4, the probability of one swing vote is: 

    D = MultinomialDistribution(100 - 1, (0.4, 0.4, 1 - 0.4 - 0.4));
    Probability(x == y, (x, y, z) -> D)
    -> 0.0447288


  Probability that a winner won by one vote:

    Probability( Abs(x - y) == 1, (x, y, z) -> D)
    -> 0.0888996

  Probability that candidate 1 wins the election:
    Probability(x > y, (x, y, z) -> D)
    ->  0.477636
  """

  variable : x
  mean: 39.599999999999945
  HPD interval (0.84): 32..45

  variable : y
  mean: 39.59999999999995
  HPD interval (0.84): 32..45

  variable : z
  mean: 19.799999999999986
  HPD interval (0.84): 13..24

  variable : p_swing_vote
  mean: 0.044728810973981147

  variable : p_won_by_one_vote
  mean: 0.08889956800751814

  variable : p_candidate_1_wins
  mean: 0.47763559451300863


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

   (define v (multinomial_dist 99 '(0.4 0.4 0.2)))

   ;; (define (x,y,z) = v ;; This does not work
   (define x (list-ref v 0))
   (define y (list-ref v 1))
   (define z (list-ref v 2))
   
   (define p_swing_vote (= x y))
   (define p_won_by_one_vote (= (abs (- x y)) 1))
   (define p_candidate_1_wins (> x y))
   
   (list v
         x
         y
         z
         p_swing_vote
         p_won_by_one_vote
         p_candidate_1_wins
         )
   )
)

(show-marginals (model)
                (list  "v"
                       "x"
                       "y"
                       "z"
                       "p_swing_vote"
                       "p_won_by_one_vote"
                       "p_candidate_1_wins"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


