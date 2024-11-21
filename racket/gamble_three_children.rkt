#| 

  Three children paradox in Racket/Gamble 

  From https://ali.medium.com/3-probability-paradoxes-challenging-our-understanding-of-chance-01d950affc9b
  """  
  Let’s consider a family with three children and analyze the probability that all 
  these children will be of the same gender. Intuitively, one might think that since 
  the first two children are bound to be of the same gender, the odds of the third child 
  being the same gender should be 1/2.

  However, a closer examination of the probability reveals a different story. If we represent 
  girls by ‘G’ and boys by ‘B,’ there are eight possible combinations: BBB, BBG, BGB, GBB, 
  BGG, GBG, GGB, and GGG. Out of these combinations, only BBB and GGG consist of all 
  boys or all girls, respectively, meaning the actual probability of all three children 
  being the same gender is 2/8 or 1/4, not 1/2, as initially assumed.
  """

  variable : children
  (girl girl girl): 1/8 (0.125)
  (girl girl boy): 1/8 (0.125)
  (boy boy girl): 1/8 (0.125)
  (boy boy boy): 1/8 (0.125)
  (girl boy boy): 1/8 (0.125)
  (boy girl girl): 1/8 (0.125)
  (boy girl boy): 1/8 (0.125)
  (girl boy girl): 1/8 (0.125)

  variable : p
  #f: 3/4 (0.75)
  #t: 1/4 (0.25)
  mean: 1/4 (0.25)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (enumerate

   (define n 3)
   (define children (for/list ([i n]) (categorical-vw2 (vector 1/2 1/2) (vector "boy" "girl"))))

   (define p (= (length (remove-duplicates children)) 1))

   (list children
         p)

   )
)

(show-marginals (model1)
                (list  "children"
                       "p"
                       ))


#|
  A slightly different wording (which is indicated by the original question):

  Given that the two first children have the same gender, what it the
  probability that the third child also have the same gender.

  Now the probability is 1/2.

  variable : children
  (girl girl girl): 1/4 (0.25)
  (girl girl boy): 1/4 (0.25)
  (boy boy girl): 1/4 (0.25)
  (boy boy boy): 1/4 (0.25)

  variable : p
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

|#
(define (model2)
  (enumerate

   (define n 3)
   (define children (for/list ([i n]) (categorical-vw2 (vector 1/2 1/2) (vector "boy" "girl"))))

   (observe/fail (eq? (first children) (second children)))
   
   (define p (= (length (remove-duplicates children)) 1))

   (list children
         p)

   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "children"
                       "p"
                       ))


#|
  """
  Now, let’s expand this scenario to a family with four children. Which 
  scenario is more probable — having three children of one gender and the 
  fourth of the other or having two boys and two girls? At first guess, 
  many people would assume that the more balanced split of two boys and two 
  girls is more probable. But by listing out all the possible combinations, 
  we find that there are six cases where there are two boys and two girls and 
  eight cases where there is a three-to-one gender split. This means the probability 
  is actually 1/2 that there is a higher likelihood of having three children of 
  one gender and a fourth child of the other gender. This interesting paradox serves 
  as yet another example of how our intuitive understanding of probability can sometimes 
  fall short of mathematical reality.
  """

  variable : children
  (girl girl girl girl): 1/16 (0.0625)
  (girl boy girl girl): 1/16 (0.0625)
  (girl boy girl boy): 1/16 (0.0625)
  (girl boy boy boy): 1/16 (0.0625)
  (girl girl girl boy): 1/16 (0.0625)
  (boy girl girl girl): 1/16 (0.0625)
  (girl girl boy girl): 1/16 (0.0625)
  (girl girl boy boy): 1/16 (0.0625)
  (boy girl girl boy): 1/16 (0.0625)
  (girl boy boy girl): 1/16 (0.0625)
  (boy boy girl boy): 1/16 (0.0625)
  (boy boy boy boy): 1/16 (0.0625)
  (boy girl boy boy): 1/16 (0.0625)
  (boy boy girl girl): 1/16 (0.0625)
  (boy boy boy girl): 1/16 (0.0625)
  (boy girl boy girl): 1/16 (0.0625)

  variable : h
  #hash((boy . 2) (girl . 2)): 3/8 (0.375)
  #hash((boy . 1) (girl . 3)): 1/4 (0.25)
  #hash((boy . 3) (girl . 1)): 1/4 (0.25)
  #hash((girl . 4)): 1/16 (0.0625)
  #hash((boy . 4)): 1/16 (0.0625)

  variable : p_two_each
  #f: 5/8 (0.625)
  #t: 3/8 (0.375)
  mean: 3/8 (0.375)

  variable : p_one_three
  #f: 3/4 (0.75)
  #t: 1/4 (0.25)
  mean: 1/4 (0.25)


|#
(define (model3)
  (enumerate

   (define n 4)

   (define children (for/list ([i n]) (categorical-vw2 (vector 1/2 1/2) (vector "boy" "girl"))))
   (define h (collect children))
  
   (define p_two_each (= (hash-ref h "boy" 0) 2))
   (define p_one_three (or (= (hash-ref h "boy" 0) 1 ) (= (hash-ref h "girl" 1) 0)))

   (list children
         h
         p_two_each
         p_one_three)

   )
)

(displayln "\nModel 3")
(show-marginals (model3)
                (list  "children"
                       "h"
                       "p_two_each"
                       "p_one_three"
                       ))


