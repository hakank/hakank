#| 

  Either a spade or an ace in Racket/Gamble 

  From http://www.statistics101.net/statistics101web_000007.htm
  """
  From CliffsQuickReview Statistics, p. 43, example 5 
  What is the probability of drawing either a spade 
  or an ace from a deck of cards? 
  Calculated result: 16/52 = 0.308 
  """

  p(spade) + p(ace) - p(ace of spades):
  13/52 + 4/52 - 1/52 = 0.30769230769230769231

  Model 1
  var : card
  0: 9/13 (0.6923076923076923)
  1: 4/13 (0.3076923076923077)
  mean: 4/13 (0.3076923076923077)

  var : p
  #f: 9/13 (0.6923076923076923)
  #t: 4/13 (0.3076923076923077)
  mean: 4/13 (0.3076923076923077)


  Model 2
  var : space
  #f: 3/4 (0.75)
  #t: 1/4 (0.25)
  mean: 1/4 (0.25)

  var : ace
  #f: 12/13 (0.9230769230769231)
  #t: 1/13 (0.07692307692307693)
  mean: 1/13 (0.07692307692307693)

  var : p
  #f: 9/13 (0.6923076923076923)
  #t: 4/13 (0.3076923076923077)
  mean: 4/13 (0.3076923076923077)



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

   (define card (categorical-vw2 (vector 16 36) (vector 1 0))) ; 13 spade + 4 aces - 1 ace of spade = 16
   (define p (= card 1))

   (list card
         p)   
   )
  )

(displayln "Model 1")
(show-marginals (model)
                (list  "card"
                       "p"))


#|
  Another approach

|#
(define (model2)
  (enumerate
   
   (define card (random-integer 52))
   (define spade (< card 13)); Is this a spade?
   (define ace (= (modulo card 13) 0)) ; This is an ace
   (define p (or spade ace))
   
   (list ; card
         spade
         ace
         p
         )
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  ; "card"
                       "space"
                       "ace"
                       "p"))

