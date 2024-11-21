#| 

  Sultan't children in Racket/Gamble 

  From "Puzzle-based learning", page 80
  """
  Many years ago, a powerful sultan had the largest harem in the 
  world. He had many children with his many wifes, and toward the 
  end of his life the total number of his children was estimated
  to be between 100 and 500. However, the sultan kept the exact number
  of his children a secret: no one could provide a better estimation of
  this number.

  One day a foreign diplomat overheard a conversation between the sultan
  and his vizier. The sultan said: "If you select any two of my children
  at random, the probability that you selected two boys would be exactly
  50 percent.
  
  This piece of information was sufficient for the diplomat to calculate
  the exact number of the sultan's children. How many children did
  the sultan have?
  """

  The sultan have 120 children, 85 boys and 35 girls.

  variable : b
  85: 1 (1.0)
  mean: 85 (85.0)

  variable : g
  35: 1 (1.0)
  mean: 35 (35.0)

  variable : tot
  120: 1 (1.0)
  mean: 120 (120.0)

  variable : prop
  17/24: 1 (1.0)
  mean: 17/24 (0.7083333333333334)

  

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

   (define b (add1 (random-integer 400))) ; 100..500
   (define g (add1 (random-integer 400)))
   (define tot (+ b g))
   (observe/fail (and (>= tot 100) (<= tot 400)))
   ; The equation from page 81 (op.cit)
   (observe/fail (= (/ (* b (- b 1)) (* (+ b g) (+ b g -1))) 1/2))
   (define prop (/ b tot)) ; proportion of the number of boys to the total

   (list b
         g
         tot
         prop
         )
   )
)

(show-marginals (model)
                (list  "b"
                       "g"
                       "tot"
                       "prop"
                     ))


