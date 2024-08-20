#| 

  Three card problem in Racket Gamble.

  https://twitter.com/MathsEdIdeas/status/1769785845970796935
  """
  Three cards are dealt face down from a normal deck. Two are turned over and 
  are the same colour. What is the probability that the third card is the same 
  colour as the first two?

  HT Martin Gardner @WWMGT, but was his solution correct?

   [ 
     9 of spades    Queen of clubs     back of a card
   ]

  """

  Note: color is black or red (not clubs, hearts, spades, diamonds).

  Result (exact):
  (#f : 13/25 (0.52))
  (#t : 12/25 (0.48))

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(show-model
 (enumerate
  (define n 52)
  (define mod 2)
  (define c1 (add1 (discrete-uniform n)))
  (define c2 (add1 (discrete-uniform n)))
  (define c3 (add1 (discrete-uniform n)))
  
  (define p  (= (modulo c3 mod) (modulo c1 mod)))
  
  (observe/fail  
   (and
    ; Different numbers and
    (not (= c1 c2))
    (not (= c1 c3))
    (not (= c2 c3))
    ; The first two cards has the same color (black/white)
    (= (modulo c1 mod) (modulo c2 mod))))
  
  ; Probability that the third card is the same as the other two cardsa
  p)
 
 )
