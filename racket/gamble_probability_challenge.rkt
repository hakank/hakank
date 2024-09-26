#| 

  Probability challenge in Racket.Gamble 

  https://medium.com/illumination/can-you-solve-this-probability-challenge-6a112e661951
  """

     [

        A                          B

      Chest with 100% gold       Chest with 50% gold
                                 and 50% silver
    ]   

  You randomly choose a treasure chest to open, and then randomly choose a 
  coin from that treasure chest. If the coin you choose is gold, then what 
  is the probability that you chose chest A?"

  ...

  Solution 1

  Here’s an intuitive explanation.

  There are two ways to get a gold coin: from chest A (from which we have a 
  100% chance), or from chest B (from which we have a 50% chance). Since we 
  are twice as likely to get a gold coin if we are choosing from chest A, 
  the odds that we choose from chest A are 2:1. So the probability is 2/3.

  ...

  But wait Captain, here’s a bonus problem:

    A family has two children. Given that one of the children is a boy, 
    what is the probability that both children are boys?

  """

  * Chest problem

  var : chest
  A: 2/3 (0.6666666666666666)
  B: 1/3 (0.3333333333333333)

  var : pick
  gold: 1 (1.0)

  Cf 
  - gamble_bertrands_paradox.rkt 
  - gamble_bertrands_paradox_resampling.rkt 


  * Bonus problem
    See below 

  For the bonus problem, also see
  - gamble_daughter_or_son.rkt
  - gamble_two_children_problem.rkt
  - gamble_how_many_sons.rkt


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   
   (define chest (uniform-draw (list "A" "B")))
   (define pick
     (if (eq? chest "A")
         (categorical-vw2 (vector 100 0) (vector "gold" "silver"))
         (categorical-vw2 (vector 50 50) (vector "gold" "silver"))))
     
   (observe/fail (eq? pick "gold"))

   (list chest
         pick)
   )
)

(show-marginals (model)
                (list  "chest"
                       "pick"))



#|
  Bonus problem
  """
  A family has two children. Given that one of the children is a boy, 
  what is the probability that both children are boys?
  """

  Bonus problem:
  var : child1
  boy: 2/3 (0.6666666666666666)
  girl: 1/3 (0.3333333333333333)

  var : child2
  boy: 2/3 (0.6666666666666666)
  girl: 1/3 (0.3333333333333333)

  var : p
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)


|#
(define (model2)
  (enumerate
   
   (define child1 (categorical-vw2 (vector 1/2 1/2) (vector "boy" "girl")))
   (define child2 (categorical-vw2 (vector 1/2 1/2) (vector "boy" "girl")))

   ; "One of the children is a boy"
   (observe/fail (or (eq? child1 "boy") (eq? child2 "boy")))

   (define p (and (eq? child1 "boy") (eq? child2 "boy")))
   
   (list child1
         child2
         p)
   )
)

(displayln "\nBonus problem:")
(show-marginals (model2)
                (list  "child1"
                       "child2"
                       "p"))
