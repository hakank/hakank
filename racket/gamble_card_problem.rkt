#| 

  Card problem in Racket Gamble.

  From https://math.stackexchange.com/questions/513250/conditional-probability-question-with-cards-where-the-other-side-color-is-to-be
   """
   A box contains three cards. One card is red on both sides, one card is green on both sides, 
   and one card is red on one side and green on the other. One card is selected from the 
   box at random, and the color on one side is observed. If this side is green, what is 
   the probability that the other side of the card is also green?

   ... 

   the answer to this question is 2/3.
   """

  Given that the color of card is green, the the probability of the different cards are:

  (all_green : 2/3 (0.6666666666666666))
  (red_green : 1/3 (0.3333333333333333))
 
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (card-problem)
  (enumerate
   
   ;;  What card did we select?
   (define selected-card (categorical-vw (vector "all_red" "all_green" "red_green") (vector 1/3 1/3 1/3)))
 
   ;;  What is the color of the card (one side) that we see?
   (define card
     (case selected-card
       [("all_red") "red"]
       [("all_green") "green"]
       [("red_green") (categorical-vw (vector "red" "green") (vector 1/2 1/2))])
     )
   
   ;;  The color of the card we see is green
   (observe/fail (eq? card "green"))

   ;; What do we want to know?
   selected-card)  
  )

(show-model (card-problem))
