#| 

  Monty Hall problem in Racket Gamble.

  From the PyMC3 model in the talk
  "Carlo Revolution is Open Source: Probabilistic Programming with PyMC3?Austin Rochford"
  https://www.safaribooksonline.com/videos/open-data-science/9780135432792/9780135432792-ODSC_11
  Around time 9:00

  The assumptions are: 
  - the prize is at random place
  - we always select door 1
  - Monty Hall opens door 2

  var : prize
  3: 2/3 (0.6666666666666666)
  1: 1/3 (0.3333333333333333)
  mean: 7/3 (2.3333333333333335)

  Which mean that if we had kept door d1 (i.e. didn't switch to door d3) it will be 1/3 chance 
  of being the price door.
  Changing to d3 would - however - give a 2/3 change of getting the price.

  Cf gamble_monty_hall_problem.rkt

  This is a port of my WebPPL model monty_hall.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  (enumerate
   
   (define d1 1)
   (define d2 2)
   (define d3 3)

   (define doors (list d1 d2 d3))

   
   ; The prize can be behind any door 1..3.
   (define prize (uniform-draw doors))
    
    ; Which door will Monty open?
    ; Assumption (WLOG): We always select door 1.    
   (define open (if (= prize d1)
                    (categorical-vw (vector d2 d3) (vector 1/2 1/2))
                    (if (= prize d2) d3 d2)
                    )
     )
    
    ; We see that Monty opens door 2.
    (observe/fail (= open d2))
    
    ; What are the probabilities that the price is behind
    ; - door d1 (the one we selected, i.e don't switch)
    ; - or door d3 (i.e. switch door)
    (list prize)
    
    )

  )


(show-marginals (model)
                (list "prize")
                
                )
