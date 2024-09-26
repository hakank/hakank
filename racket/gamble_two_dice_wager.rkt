#| 

  Two dice wager in Racket.Gamble 

  From https://puzzlewocky.com/brain-teasers/the-two-dice-wager/
  """
  A gambler presents you with an even-money wager. You will roll two dice, and if the highest 
  number showing is one, two, three or four, then you win. If the highest number on either die 
  is five or six, then she wins. Should you take the bet?
  """

  No, this is not a fair game. The 1..4 player will win 4/9 of the games and lose 5/9 of
  the game.

  var : m
  6: 11/36 (0.3055555555555556)
  5: 1/4 (0.25)
  4: 7/36 (0.19444444444444445)
  3: 5/36 (0.1388888888888889)
  2: 1/12 (0.08333333333333333)
  1: 1/36 (0.027777777777777776)
  mean: 161/36 (4.472222222222222)

  var : p1_4
  #f: 5/9 (0.5555555555555556)
  #t: 4/9 (0.4444444444444444)
  mean: 4/9 (0.4444444444444444)

  var : p5_6
  #t: 5/9 (0.5555555555555556)
  #f: 4/9 (0.4444444444444444)
  mean: 5/9 (0.5555555555555556)

  However, it game be made more fair if we add weights to the wins: 5/9 and 4/9 respectively.

  Fair game: Checking the result of model2 with mult1=5/9  and mult2=4/9
  var : m
  6: 11/36 (0.3055555555555556)
  5: 1/4 (0.25)
  4: 7/36 (0.19444444444444445)
  3: 5/36 (0.1388888888888889)
  2: 1/12 (0.08333333333333333)
  1: 1/36 (0.027777777777777776)
  mean: 161/36 (4.472222222222222)

  var : p1_4
  0: 5/9 (0.5555555555555556)
  5/9: 4/9 (0.4444444444444444)
  mean: 20/81 (0.24691358024691357)

  var : p5_6
  4/9: 5/9 (0.5555555555555556)
  0: 4/9 (0.4444444444444444)
  mean: 20/81 (0.24691358024691357)

  Now, the expected outcome of the two players are the same.


  This is a port of my WebPPL model two_dice_wager.rkt

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
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define d1 (add1 (random-integer 6)))
   (define d2 (add1 (random-integer 6)))
   (define m (max d1 d2))
   (define p1_4 (<= m 4))
   (define p5_6 (>= m 5))

   (list ; d1
         ; d2
         m
         p1_4
         p5_6
         )
   
   )
)

(show-marginals (model)
                (list  ; "d1"
                       ; "d2"
                       "m"
                       "p1_4"
                       "p5_6"))


(define (model2 mult1 mult2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define d1 (add1 (random-integer 6)))
   (define d2 (add1 (random-integer 6)))
   (define m (max d1 d2))
   (define p1_4 (* mult1(boolean->integer (<= m 4))))
   (define p5_6 (* mult2 (boolean->integer (>= m 5))))
   
   (list ; d1
         ; d2
         m
         p1_4
         p5_6
         )
   
   )
  )

(displayln "Fair game: Checking the result of model2 with mult1=5/9  and mult2=4/9")
(show-marginals (model2 5/9 4/9)
                (list  ; "d1"
                       ; "d2"
                       "m"
                       "p1_4"
                       "p5_6"))
