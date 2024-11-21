#| 

  BrainTwister #44: Dice and cards in Racket/Gamble 

  https://enigmaticcode.wordpress.com/2024/11/01/braintwister-44-dice-and-cards/
  """
  From New Scientist #3515, 2nd November 2024
  
  On each turn of a game, we roll three standard dice with faces numbered 1 to 6 and 
  add up the numbers shown.
  (a) What is the average (mean) value of that total over a long game?

  Now suppose instead of rolling dice, we draw three cards from a six-card deck 
  where the cards are numbered 1 to 6.
  (b) If we put each card back after drawing it and shuffle before drawing another card, 
      does this change the expected value?

  (c) If we instead draw three cards without replacement (so the probabilities are no longer 
      independent), what is the average value of the total?
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


#|
  """
  On each turn of a game, we roll three standard dice with faces numbered 1 to 6 and 
  add up the numbers shown.
  (a) What is the average (mean) value of that total over a long game?
  """

  variable : s
  10: 1/8 (0.125)
  11: 1/8 (0.125)
  9: 25/216 (0.11574074074074074)
  12: 25/216 (0.11574074074074074)
  8: 7/72 (0.09722222222222222)
  13: 7/72 (0.09722222222222222)
  7: 5/72 (0.06944444444444445)
  14: 5/72 (0.06944444444444445)
  6: 5/108 (0.046296296296296294)
  15: 5/108 (0.046296296296296294)
  16: 1/36 (0.027777777777777776)
  5: 1/36 (0.027777777777777776)
  17: 1/72 (0.013888888888888888)
  4: 1/72 (0.013888888888888888)
  18: 1/216 (0.004629629629629629)
  3: 1/216 (0.004629629629629629)
  mean: 21/2 (10.5)

|#
(define (model-a)
  (enumerate

   (define n 3)

   (define x (for/list ([i n]) (add1 (random-integer 6))))
   (define s (sum x))

   (list s)
   
   )
)

(displayln "Problem a)")
(show-marginals (model-a)
                (list  "s"))


#|
  """
  Now suppose instead of rolling dice, we draw three cards from a six-card deck 
  where the cards are numbered 1 to 6.
  (b) If we put each card back after drawing it and shuffle before drawing another card, 
      does this change the expected value?
  """

  No, the estimated value is the same as in a).

  variable : s
  10: 1/8 (0.125)
  11: 1/8 (0.125)
  9: 25/216 (0.11574074074074074)
  12: 25/216 (0.11574074074074074)
  8: 7/72 (0.09722222222222222)
  13: 7/72 (0.09722222222222222)
  7: 5/72 (0.06944444444444445)
  14: 5/72 (0.06944444444444445)
  6: 5/108 (0.046296296296296294)
  15: 5/108 (0.046296296296296294)
  16: 1/36 (0.027777777777777776)
  5: 1/36 (0.027777777777777776)
  17: 1/72 (0.013888888888888888)
  4: 1/72 (0.013888888888888888)
  18: 1/216 (0.004629629629629629)
  3: 1/216 (0.004629629629629629)
  mean: 21/2 (10.5)

|#

(define (model-b)
  (enumerate

   (define n 3)

   (define x (resample n (range 1 7)))
   (define s (sum x))

   (list s)
   
   )
)
(displayln "\nProblem b)")
(show-marginals (model-b)
                (list  "s"))



#|
  """
  (c) If we instead draw three cards without replacement (so the probabilities are no longer 
      independent), what is the average value of the total?
   """

  This is also the same estimated value.

  variable : s
  9: 3/20 (0.15)
  10: 3/20 (0.15)
  11: 3/20 (0.15)
  12: 3/20 (0.15)
  8: 1/10 (0.1)
  13: 1/10 (0.1)
  6: 1/20 (0.05)
  7: 1/20 (0.05)
  14: 1/20 (0.05)
  15: 1/20 (0.05)
  mean: 21/2 (10.5)


|#
(define (model-c)
  (enumerate

   (define n 3)

   (define x (draw-without-replacement n (range 1 7)))
   (define s (sum x))

   (list s)
   
   )
)
(displayln "\nProblem c)")
(show-marginals (model-c)
                (list  "s"))

