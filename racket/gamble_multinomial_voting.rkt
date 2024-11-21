#| 

  Multinomial voting in Racket/Gamble 

  Multinomial example voting
  From https://en.wikipedia.org/wiki/Multinomial_distribution#Example
  """
  Suppose that in a three-way election for a large country, candidate A received 20% of 
  the votes, candidate B received 30% of the votes, and candidate C received 50% of 
  the votes. 

  If six voters are selected randomly, what is the probability that there will be exactly 
  one supporter for candidate A, two supporters for candidate B and three supporters for 
  candidate C in the sample?

  Note: Since we’re assuming that the voting population is large, it is reasonable 
  and permissible to think of the probabilities as unchanging once a voter is selected 
  for the sample. Technically speaking this is sampling without replacement, so the 
  correct distribution is the multivariate hypergeometric distribution, but the 
  distributions converge as the population grows large.

   Pr = ... = 0.135 
   """

   variable : m
   (1 2 3): 0.13499999999999993
   (1 1 4): 0.11249999999999996
   (2 1 3): 0.09000000000000001
   (0 2 4): 0.08437500000000002
   (2 2 2): 0.08100000000000004
   ...
   (4 2 0): 0.0021600000000000005
   (5 0 1): 0.0009599999999999997
   (0 6 0): 0.0007289999999999991
   (5 1 0): 0.0005759999999999999
   (6 0 0): 6.400000000000006e-5

   variable : p
   #f: 0.8649999999999999
   #t: 0.13499999999999993
   mean: 0.13499999999999993

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

   ; % of votes for A,B, and C
   (define pct '(20/100 30/100 50/100)) ; Must sum to 1

   ; There are 6 votes
   (define m (multinomial_dist 6 pct))

   ;; Probability of 1 vote for A, 2 votes for B, and 3 votes for C.
   (define p (equal? m '(1 2 3)))

   (list m
         p
         )
   )
)

(show-marginals (model)
                (list  "m"
                       "p"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


