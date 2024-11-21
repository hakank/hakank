#| 

  Three men with hats in Racket/Gamble 

  From Introduction to Probability Models by Sheldon Ross
  page 8
  """
  Example 1.8 
  Suppose that each of three men at a party throws his hat into the center 
  of the room. The hats are first mixed up and then each man randomly 
  selects a hat. What is the probability that none of the three men 
  selects his own hat?
  """

  Solution: 1/3 (variable p in the model)

  variable : a
  1: 1/3 (0.3333333333333333)
  2: 1/3 (0.3333333333333333)
  3: 1/3 (0.3333333333333333)
  mean: 2 (2.0)

  variable : b
  1: 1/3 (0.3333333333333333)
  2: 1/3 (0.3333333333333333)
  3: 1/3 (0.3333333333333333)
  mean: 2 (2.0)

  variable : c
  1: 1/3 (0.3333333333333333)
  2: 1/3 (0.3333333333333333)
  3: 1/3 (0.3333333333333333)
  mean: 2 (2.0)

  variable : p
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)

  variable : p2
  #t: 2/3 (0.6666666666666666)
  #f: 1/3 (0.3333333333333333)
  mean: 2/3 (0.6666666666666666)

  variable : p3
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


(define (model)
  (enumerate

   (define ps (vector 1/3 1/3 1/3))
   (define vs (vector 1 2 3))
   (define a (categorical-vw2 ps vs))
   (define b (categorical-vw2 ps vs))
   (define c (categorical-vw2 ps vs))   

   (observe/fail (and (not (= a b)) (not (= a c)) (not (= b c))))
    
   (define p (and (not (= a 1)) (not (= b 2)) (not (= c 3)))); None get their own hat: 2/6=1/3

   (define p2 (or (= a 1) (= b 2) (= c 3))) ; At least one find their hat: 2/3

   (define p3 (= (+ (b2i (= a 1)) (b2i (= b 2)) (b2i (= c 3))) 1)) ; Exactly one get their own hat: 0.5
    
   (list a
         b
         c
         p
         p2
         p3
         )
   
   )
)

(show-marginals (model)
                (list  "a"
                       "b"
                       "c"
                       "p"
                       "p2"
                       "p3"
                     ))
