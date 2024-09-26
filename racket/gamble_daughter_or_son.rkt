#| 

  Daughter or son in Racket.Gamble 

  From https://brainstellar.com/puzzles/probability/25
  """
  Sheldon says "Suppose I have two children. Younger one is a girl". What is 
  the probability that both children are girls?

  "Forget all that, and suppose I have two children, and atleast one of them 
  is a boy". Find probability of two boys.

  "Suppose I have two kids, lets call them Bouba and Kiki", says Dr. Cooper, 
  "and suppose Bouba is a girl !" What is the probability that I have two daughters?"
  """

  (Note: The Answer states 1/2, 1/3, 1/3 but in the Solution, the third
  is stated to be 1/2. See Comments on this.)

  Here are three models for each of the three questions:
  Question 1: 1/2
  Question 2: 1/3
  Question 3: 1/2


  Cf 
  - gamble_two_children_problem.rkt
  - gamble_how_many_sons.rkt

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

#|
  """
  Sheldon says "Suppose I have two children. Younger one is a girl". What is 
  the probability that both children are girls?
  """

  var : child1
  girl: 29/40 (0.725)
  boy: 11/40 (0.275)

  var : child2
  girl: 31/40 (0.775)
  boy: 9/40 (0.225)

  var : youngest
  girl: 1 (1.0)
 
  var : p
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  I.e. the probability that both children are girls is 1/2.

|#
(define (model1)
  (enumerate

   (define (gender c) (if (flip 1/2) "boy" "girl"))
   (define (age c) (add1 (random-integer 10)))
   
   (define child1 (gender 1))
   (define child2 (gender 2))

   (define age1 (age 1))
   (define age2 (age 2))
   
   (define youngest (if (< (age 1) (age 2)) child1 child2))
   
   (define p (and (eq? child1 "girl") (eq? child2 "girl")))
   
   (observe/fail (eq? youngest "girl"))
   
   (list child1
         child2
         youngest
         p
         )
   
   )
  )

(displayln "\n* Model 1")
(show-marginals (model1)
                (list  "child1"
                       "child2"
                       "youngest"
                       "p"))


#|
  """
  "Forget all that, and suppose I have two children, and atleast one of them 
  is a boy". Find probability of two boys.
  """

  var : child1
  boy: 2/3 (0.6666666666666666)
  girl: 1/3 (0.3333333333333333)

  var : child2
  boy: 2/3 (0.6666666666666666)
  girl: 1/3 (0.3333333333333333)

  var : p
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)

  The probability that both children are boys is 1/3.
  

|#
(define (model2)
  (enumerate

   (define (gender c) (if (flip 1/2) "boy" "girl"))
   
   (define child1 (gender 1))
   (define child2 (gender 2))

   (define p (and (eq? child1 "boy") (eq? child2 "boy")))
   
   (observe/fail (or (eq? child1 "boy") (eq? child2 "boy")))
   
   (list child1
         child2
         p
         )
   
   )
  )

(displayln "\n* Model 2")
(show-marginals (model2)
                (list  "child1"
                       "child2"
                       "p"))


#|
  """
  "Suppose I have two kids, lets call them Bouba and Kiki", says Dr. Cooper, 
  "and suppose Bouba is a girl !" What is the probability that I have two daughters?"
  """

  var : bouba
  girl: 1 (1.0)

  var : kiki
  girl: 1/2 (0.5)
  boy: 1/2 (0.5)

  var : p
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  The case is the same as in case 1, i.e. probability is 1/2.

|#
(define (model3)
  (enumerate

   (define (gender c) (if (flip 1/2) "boy" "girl"))
   
   (define bouba (gender 1))
   (define kiki (gender 2))
   
   (define p (and (eq? bouba "girl") (eq? kiki "girl")))
   
   (observe/fail (eq? bouba "girl"))
   
   (list bouba
         kiki
         p
         )
   
   )
  )

(displayln "\n* Model 3")
(show-marginals (model3)
                (list  "bouba"
                       "kiki"
                       "p"))


