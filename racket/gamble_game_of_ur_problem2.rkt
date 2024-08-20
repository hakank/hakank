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


var : len
6: 0.22440000000000002
7: 0.19850000000000004
5: 0.19400000000000003
8: 0.13590000000000002
9: 0.08400000000000002
4: 0.07720000000000002
10: 0.04630000000000001
11: 0.021000000000000005
12: 0.011300000000000001
13: 0.005300000000000001
14: 0.0012000000000000001
15: 0.0007000000000000001
16: 0.00020000000000000004
mean: 6.786900000000001

  This is a port of my WebPPL model game_of_ur_problem2.wppl
  using recursion and list..


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (game-of-ur-problem)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define (roll a)
     (let [(s (sum a))]
       (if (= s 13)
           a
           (if (> s 13)
               '()
               (roll (cons (random-integer 5) a))))))
               
    (define a (roll '()))
    
    (define sum-roll (sum a))
    
    (observe/fail (= sum-roll 13))

    (list (length a))
    
   )
  )

(show-marginals (game-of-ur-problem)
                (list "len")
                #:num-samples 10000
                )

