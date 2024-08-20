#| 

  Infinite dice Racket Gamble.

  https://medium.com/mathadam/this-math-puzzle-looks-hard-fa8ec6e8ed5
  """
  This Math Puzzle Looks Hard
  Infinitely-sided dice produce a surprising result.

  Here’s the challenge. You have three infinitely-sided dice. When you roll one 
  of these dice, you get a Real Number between 0 and 1. So when you roll three of 
  these dice, you get a Real Number between 0 and 3. Capish?

  Here’s the question. You roll the three dice. You square the outcome of each die.

  What are the odds that the sum of the three squares will be less than or equal to 1?
  
  ...

  
  We need the volume of that chunk (an eight) of the sphere, and the volume of the cube. 
  Divide one by the other, and we’re done.

   1/8*(Volume of Sphere)   1/8*(Pi*4/3 * 1^3)
   ---------------------- = ------------------ = Pi / 6 ~ 0.52
     Volume of Cube              1^3

  The odds are slightly in favour of the result being less than 1.

  Isn’t it awesome that those infinitely-sided dice — which would each be spherical — 
  produce π in this way?
  """

  var : p
  #t: 0.5239
  #f: 0.4761
  mean: 0.5239


  > (/ pi 6)
  0.5235987755982988


  This is a port of my WebPPL infinite_dice.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define a (uniform 0 1))
   (define b (uniform 0 1))
   (define c (uniform 0 1))
   
   (define s (+ (* a a) (* b b) (* c c)))
   
   (define p (< s 1))
     
   (list p )

   )
  )

(show-marginals (model)
                  (list "p"
                        )
                  #:num-samples 10000
                  #:truncate-output 4
                  ; #:skip-marginals? #t
                  ; #:show-histogram? #t
                  )
