#| 

  Guessing game in Racket Gamble.

   https://social.microsoft.com/Forums/en-US/a76a904d-ae2e-4118-bec0-c928772d7ff7/solving-the-nested-number-guessing-problem-from-anglican-web-site?forum=infer.net
  """
  I'm trying to express with Infer.NET one of the examples in the Anglican web site:
     http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=nested-number-guessing

  Here the description of the problems:
    "Two agents play a game in which each agent needs to name a number between 0 and 9 and they win
     if their numbers add up to 13. The first player knows this, and he knows that the second player
     gets to see the number the first player chooses, but the second player mistakenly thinks that
     the two win if their numbers add up to any number greater than 8 (and the first player knows
     this as well). What number should the first player choose?
  """ 

  Cf the Church model http://forestdb.org/models/nested-guessing.html
  """
  (define (sample)
  (rejection-query
   (define a (sample-integer 10))
   (define b
     (rejection-query
      (define c (sample-integer 10))
      c
      (> (+ a c) 8)))
   a
   (= (+ a b) 13)))

  (hist (repeat 10000 sample))
  """


  This Gamble model gives the following probabilities for a, i.e. that player 
  1 should play number 4:
  
  (4 : 504/2131 (0.23650868137024872))
  (5 : 420/2131 (0.1970905678085406))
  (6 : 360/2131 (0.1689347724073205))
  (7 : 315/2131 (0.14781792585640544))
  (8 : 280/2131 (0.1313937118723604))
  (9 : 252/2131 (0.11825434068512436))
  (mean: 6.095260441107461)

  Cf 
  * my WebPPL model number_guessing_game.wppl
  * my PSI model number_guessing_game.psi

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (guessing-game)
  (enumerate
   
   (define a (random-integer 10))
 
   (define b
     ; Note sample here
     (sample (enumerate
      (define c (random-integer 10))
      (observe/fail (> (+ a c) 8))
      c)
     ))
   
   (observe/fail (= (+ a b) 13))

   a
   ; (list a b)
   )
  )

(show-model (guessing-game))
