#| 

  Guess the Toss in Racket.Gamble 

  From https://brainstellar.com/puzzles/probability/108
  """
  A and B are in a team called AB, playing against C. If AB team 
  wins they win Rs 3, nothing otherwise.

  The Game: A and B are placed in 2 separate rooms far away. A will 
  toss a coin and B will also toss a coin; A will have to guess the 
  outcome of B's toss and B will guess A's. If both guesses are right, 
  team AB wins Rs 3, nothing otherwise. Should they play the game, 
  by paying Rs 1 at the start?

  Answer: 1/2
  Solution: Let's list down all the possibilities.
      A B
      H H
      H T
      T H
      T T
  Observe that 2 out of 4 times, their coins have the same face, i.e. either 
  both heads or both tails.

  They can speak their own coin's face as their guess. They win the game with a probability of 
  1/2.

  The pay-off will be positive (1/2)*3 - 1 = 0.5
  and hence they should play this game.
  """

  My initial take was the no-strategy which is a loss of 1/4.
  With the strategy mentioned, it's a gaom of 1/2.

  var : outcome-no-strategy
  0: 3/4 (0.75)
  3: 1/4 (0.25)
  mean: 3/4 (0.75)

  var : profit-no-strategy
  -1: 3/4 (0.75)
  2: 1/4 (0.25)
  mean: -1/4 (-0.25)

  var : outcome-strategy
  0: 1/2 (0.5)
  3: 1/2 (0.5)
  mean: 3/2 (1.5)

  var : profit-strategy
  -1: 1/2 (0.5)
  2: 1/2 (0.5)
  mean: 1/2 (0.5)


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

   (defmem (toss i) (if (flip) "H" "T"))
   (defmem (guess i) (if (flip) "H" "T"))   
   
   (define a-toss (toss 0) )
   (define b-toss (toss 1) )   

   (define a-guess (guess 0))
   (define b-guess (guess 1))
  
   (define a-guess2 a-toss)
   (define b-guess2 b-toss)
  

   (define outcome-no-strategy (if (and (eq? a-toss b-guess) (eq? b-toss a-guess))
                                   3
                                   0))

   (define profit-no-strategy (- outcome-no-strategy 1))

   ; Strategy: guess the same as your own toss
   (define outcome-strategy (if (and (eq? a-toss b-guess2) (eq? b-toss a-guess2))
                                3
                                0))

   (define profit-strategy (- outcome-strategy 1))

   
   (list outcome-no-strategy
         profit-no-strategy
         outcome-strategy
         profit-strategy
         )
   
   )
)

(show-marginals (model)
                (list  "outcome-no-strategy"
                       "profit-no-strategy"
                       "outcome-strategy"
                       "profit-strategy"
                       ))

