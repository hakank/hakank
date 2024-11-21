#| 

  Newton-Pepy's problem in Racket/Gamble 

  https://en.wikipedia.org/wiki/Newton%E2%80%93Pepys_problem
  """
  The Newton–Pepys problem is a probability problem concerning the probability of throwing 
  sixes from a certain number of dice.

  In 1693 Samuel Pepys and Isaac Newton corresponded over a problem posed to Pepys by a school 
  teacher named John Smith. The problem was:

  Which of the following three propositions has the greatest chance of success?

  A. Six fair dice are tossed independently and at least one "6" appears.
  B. Twelve fair dice are tossed independently and at least two "6"s appear.
  C. Eighteen fair dice are tossed independently and at least three "6"s appear.

  Pepys initially thought that outcome C had the highest probability, but Newton correctly concluded 
  that outcome A actually has the highest probability.

  Solution
  The probabilities of outcomes A, B and C are:

  P(A): 0.6651
  P(B): 0.6187
  P(C): 0.5973
  """

  Model 1:
  variable : a
  #t: 0.6651020233196169
  #f: 0.3348979766803831
  mean: 0.6651020233196169

  variable : b
  #t: 0.6186673737323083
  #f: 0.3813326262676917
  mean: 0.6186673737323083

  variable : c
  #t: 0.5973456859477229
  #f: 0.4026543140522771
  mean: 0.5973456859477229

  Model 2 (simulation)
  variable : a
  #t: 0.6541
  #f: 0.3459
  mean: 0.6541

  variable : b
  #t: 0.6199
  #f: 0.3801
  mean: 0.6199

  variable : c
  #t: 0.5932000000000001
  #f: 0.4068
  mean: 0.5932000000000001


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (enumerate

   (define a (>= (binomial  6 1/6) 1))
   (define b (>= (binomial 12 1/6) 2))
   (define c (>= (binomial 18 1/6) 3))
   (list a b c)
   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list  "a" "b" "c"))


(define (model2)
  (importance-sampler
   (define a (>= (sum (repeat (lambda () (b2i (= (add1 (random-integer 6)) 6)))  6)) 1))
   (define b (>= (sum (repeat (lambda () (b2i (= (add1 (random-integer 6)) 6))) 12)) 2))
   (define c (>= (sum (repeat (lambda () (b2i (= (add1 (random-integer 6)) 6))) 18)) 3))
   (list a b c)
   )
)

(displayln "Model 2 (simulation)")
(show-marginals (model2)
                (list  "a" "b" "c")
                #:num-samples 10000)
