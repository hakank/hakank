#| 

  Guessing game (Church) in Racket Gamble.

  Here is a direct port of the Church model http://forestdb.org/models/nested-guessing.html
  """
  Two agents play a game in which each agent needs to name a number between 0 and 9 and they 
  win if their numbers add up to 13. The first player knows this, and he knows that the second 
  player gets to see the number the first player chooses, but the second player mistakenly thinks 
  that the two win if their numbers add up to any number greater than 8 (and the first player knows 
  this as well). What number should the first player choose?

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

  References:
  Reasoning about Reasoning by Nested Conditioning: Modeling Theory of Mind with Probabilistic 
  Programs. Andreas Stuhlmüller and Noah D. Goodman (2013). Cognitive Systems Research.
  [http://www.mit.edu/~ast/papers/nested-conditioning-cogsys2013.pdf]
  """

  Output:
  (4 : 0.24435338796721967)
  (5 : 0.1929842094743154)
  (6 : 0.16799920047971217)
  (7 : 0.14911053367979213)
  (8 : 0.13082150709574256)
  (9 : 0.11473116130321807)
  (mean: 6.073355986408155)

  The "pure" Gamble model gamble_number_guessing_game.rkt
  uses enumerate and gives the following exact probabilities:

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

(define (sample)
  (rejection-query
   ; enumeration-query
   (define a (sample-integer 10))
   (define b
     (rejection-query
      ; enumeration-query
      (define c (sample-integer 10))
      c
      #:when
      (> (+ a c) 8)))
   a
   #:when
   (= (+ a b) 13)))

; (hist (repeat 10000 sample))
(define samples (repeat (lambda () (sample)) 10000))
(show-freq samples)

(displayln "\nPercentiles:")
(show-percentiles samples)

