#| 

  Rolling the bullet in Racket Gamble.

  From https://brainstellar.com/puzzles/probability/1
  """
  Two bullets are loaded into a gun's round barrel consecutively. The barrel has 
  a capacity of 6. The gun is fired once, but no bullet is shot. Does rolling the 
  barrel (shuffling) before next shot increase the probability of firing a bullet?

  Hint:
  Since the bullets are loaded consecutively, the next shot is also constrained.

  Answer:
  Yes, shuffling increases the probability of firing a bullet from 25% to 33.3%)

  Solution:
  Initial Misstep: If the two bullets are randomly put instead of consecutively, then, 
  after firing one empty shot, there are 2 bullets and 5 total slots. The probability 
  would be 2/5=40, but that's not the case here.

  Correct step: The probability of firing a bullet without a shuffle is 1/4=25%. 
  To understand this, imagine that the firing pin was on one of the empty slots 
  (3,4,5,6), and the first shot was taken, but no bullet was fired. Now assumming that 
  the barrel rotates clockwise, the pin will move to one of these slots: (2,3,4,5). 
  Out of these four slots, only the slot (1) has a bullet. Hence probability of 
  firing a bullet is 1/4=25%. Note that the same is true in anti-clockwise direction.
  barrel

  After the shuffle, the state is reset. There are 6 total slots with 2 bullets, the 
  probabilty of firing a bullet after a shuffle is 2/6=1/3≈33%.

  Thus, shuffling does increase the probability of firing a bullet (from 25% to 33)
  """

  This model confirms the above:
  The probability of a bullet with no-rolling is 1/4 (0.25)
  The probability of a bullet with rolling is 1/3 (0.3333333333333333)
  So, yes, rolling the barrel increase the probability of shooting 
  a bullet in the second round.

operation: no-rolling
var : shot-1
2: 1/4 (0.25)
3: 1/4 (0.25)
4: 1/4 (0.25)
5: 1/4 (0.25)
mean: 7/2 (3.5)

var : res-1
empty: 1 (1.0)

var : shot-2
0: 1/4 (0.25)
3: 1/4 (0.25)
4: 1/4 (0.25)
5: 1/4 (0.25)
mean: 3 (3.0)

var : res-2
empty: 3/4 (0.75)
bullet: 1/4 (0.25)  <---


operation: rolling
var : shot-1
2: 1/4 (0.25)
3: 1/4 (0.25)
4: 1/4 (0.25)
5: 1/4 (0.25)
mean: 7/2 (3.5)

var : res-1
empty: 1 (1.0)

var : shot-2
0: 1/6 (0.16666666666666666)
1: 1/6 (0.16666666666666666)
2: 1/6 (0.16666666666666666)
3: 1/6 (0.16666666666666666)
4: 1/6 (0.16666666666666666)
5: 1/6 (0.16666666666666666)
mean: 5/2 (2.5)

var : res-2
empty: 2/3 (0.6666666666666666)
bullet: 1/3 (0.3333333333333333) <---

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (rolling-the-bullet action)
  (show "\noperation" action)
  
  (enumerate

   (define gun '("bullet" "bullet" "empty" "empty" "empty" "empty"))
   (define n (length gun))
   
   (define shot-1 (random-integer n))
   (define res-1 (list-ref gun shot-1))
   
   (define shot-2
     (if (eq? action "no-rolling")
         (modulo (add1 shot-1) n) ; the next one
         (random-integer n))      ; whatever
     )
   (define res-2 (list-ref gun shot-2))
     
   (observe/fail (eq? res-1 "empty"))

   (list shot-1 res-1 shot-2 res-2)
   
   )
  )

(show-marginals (rolling-the-bullet "no-rolling")
                (list "shot-1" "res-1" "shot-2" "res-2")
                )

(show-marginals (rolling-the-bullet "rolling")
                (list "shot-1" "res-1" "shot-2" "res-2")
                )


