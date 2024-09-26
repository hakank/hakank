#| 

  Flipping three coins in Racket/Gamble 

  From http://www.statistics101.net/statistics101web_000007.htm
  """
  From CliffsQuickReview Statistics, p. 38, example 1 
  What is the probability of simultaneously  
  flipping 3 coins and having them all land heads?  
  """

  var : heads
  1: 3/8 (0.375)
  2: 3/8 (0.375)
  0: 1/8 (0.125)
  3: 1/8 (0.125)
  mean: 3/2 (1.5)

  var : p
  #f: 7/8 (0.875)
  #t: 1/8 (0.125)
  mean: 1/8 (0.125)


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
   
   (define coin '(0 1))
   (define sample (resample 3 coin))
   (define heads (count-occurrences 1 sample))
   (define p (= heads 3))
   (list heads
         p)
   
   )
)

(show-marginals (model)
                (list  "heads"
                       "p"
                       ))


