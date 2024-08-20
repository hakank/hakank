#| 

  Game of Ur problem in Racket Gamble.

  https://www.allendowney.com/blog/2018/10/21/the-game-of-ur-problem/
  """
  Here’s a probability puzzle to ruin your week.

  In the Royal Game of Ur, players advance tokens along a track with 14 spaces. 
  To determine how many spaces to advance, a player rolls 4 dice with 4 sides. Two corners 
  on each die are marked; the other two are not. The total number of marked corners — 
  which is 0, 1, 2, 3, or 4 — is the number of spaces to advance.

  For example, if the total on your first roll is 2, you could advance a token to space 2. 
  If you roll a 3 on the next roll, you could advance the same token to space 5.

  Suppose you have a token on space 13. How many rolls did it take to get there?
   """

  See:
  https://www.allendowney.com/blog/lions-and-tigers-and-bears/

  Allen Downey's solution:
  http://nbviewer.jupyter.org/github/AllenDowney/ThinkBayes2/blob/master/solutions/game_of_ur_soln.ipynb?flush=true

var : num-rolls
7: 0.208
6: 0.2
5: 0.184
8: 0.155
9: 0.097
10: 0.055
4: 0.053
11: 0.024
12: 0.011
13: 0.008
14: 0.002
17: 0.001
18: 0.001
15: 0.001
mean: 7.028999999999999

  This is a port of my WebPPL model game_of_ur_problem.wppl


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (game-of-ur-problem)
  (; enumerate
   rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define num-rolls (+ 3 (random-integer 17)))
   (define (roll i) (random-integer 5))
   
   (define sum-rolls (for/sum ([i (range num-rolls)]) (roll i)))
    
   (observe/fail (= sum-rolls 13))

   (list num-rolls)
   )
  )

(show-marginals (game-of-ur-problem)
                (list "num-rolls")
                )

