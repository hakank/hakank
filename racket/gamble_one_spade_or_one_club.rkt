#| 

  One Spade or one Club in Racket/Gamble 

  From http://www.statistics101.net/statistics101web_000007.htm
  """
  From CliffsQuickReview Statistics, p. 41, example 3 
  What is the probability of at least one spade or one 
  club being randomly chosen in one draw from a deck 
  of cards? Calculated result: 13/52 + 13/52 = 0.5. 
  """

  Model 1: Using resampling
  variable : card
  spade: 1/4 (0.25)
  heart: 1/4 (0.25)
  diamond: 1/4 (0.25)
  club: 1/4 (0.25)

  variable : p
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  Model 2: Using enumerate
  variable : card
  spade: 1/4 (0.25)
  heart: 1/4 (0.25)
  diamond: 1/4 (0.25)
  club: 1/4 (0.25)

  variable : p
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

; Resampling
(define deck (append (rep 13 "spade") (rep 13 "club") (rep 13 "heart") (rep 13 "diamond")))
(define (model)
  (enumerate

   (define card (first (resample 1 deck)))
   (define p (or (eq? card "spade") (eq? card "club")))
   (list card p)
   )
)

(displayln "Model 1: Using resampling")
(show-marginals (model)
                (list  "card" "p"))


; Using enumerate
(define (model2)
  (enumerate

   (define card (categorical-vw2 (vector 13 13 13 13) (vector "spade" "club" "heart" "diamond")))
   (define p (or (eq? card "spade") (eq? card "club")))
   (list card p)
   )
  )

(displayln "\nModel 2: Using enumerate")
(show-marginals (model2)
                (list  "card" "p"))

