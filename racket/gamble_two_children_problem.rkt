#| 

  Two children problem in Racket.Gamble 

  https://robeastaway.com/blog/boy-girl
  """
  I have two children, at least one of whom is a boy.  
  What is the chance that the second child is also a boy?

  ...

  The classic, surprising answer is that the chance of a second 
  boy is 1/3, not 1/2 as most would expect.  Why?  Because if I 
  have two children, there is an equal chance that they will be 
  Boy-Boy, Boy-Girl, Girl-Boy or Girl-Girl.  There are three 
  equally likely combinations in which at least one child is 
  a boy (BB, BG and GB) and only in the first scenario is the 
  other child also a boy.
  """

  We assume the inital probability of a boy/girl as 0.5.

  Here we model two similiar but different problems.

  First model: 
  """
  I have two children, _at least one of whom_ is a boy.  
  What is the chance that the second child is also a boy?
  """

  Model 1
  var : child1
  boy: 2/3 (0.6666666666666666)
  girl: 1/3 (0.3333333333333333)

  var : child2
  boy: 2/3 (0.6666666666666666)
  girl: 1/3 (0.3333333333333333)

  var : two boys
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)

  var : one boy and one girl
  #t: 2/3 (0.6666666666666666)
  #f: 1/3 (0.3333333333333333)
  mean: 2/3 (0.6666666666666666)

  I.e. the probability that the other child is also a boy (i.e. two boys) is 1/3,


  Second model:
  """
  I have two children, the eldest is a boy.  
  What is the chance that the second child is also a boy?
  """

  Model 2
  var : child1
  boy: 1 (1.0)

  var : child2
  girl: 1/2 (0.5)
  boy: 1/2 (0.5)

  var : two boys
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  var : one boy and one girl
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  Since we now know that child1 is a boy, it's a matter of chance (50%) 
  if the other child is also a boy.
  
  Note:
  In gamble_how_many_sons.rkt I model the same problems, but with a 
  little different approach.

  This is a port of my WebPPl model two_children_problem.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

#|
  First model: 
  """
  I have two children, _at least one of whom_ is a boy.  
  What is the chance that the second child is also a boy?
  """
|#
(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (defmem (gender c) (categorical-vw2 (vector 1/2 1/2) (vector "boy" "girl")))

   (define child1 (gender 1))
   (define child2 (gender 2))

   (define num_boys (+ (if (eq? child1  "boy") 1 0) (if (eq? child2 "boy") 1 0)))
   
   ;; Note: it's important that we state the condition
   ;;   "at least one of whom is a boy"
   ;; as something like this.
   (observe/fail (>= num_boys 1))

   (list child1
         child2
         (= num_boys 2) ; two boys
         (= num_boys 1) ; one boy and one girl
    )

   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list  "child1"
                       "child2"
                       "two boys"
                       "one boy and one girl"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


#|
  Second model: 
  """
  I have two children, the eldest is a boy.  
  What is the chance that the second child is also a boy?
  """

  Here we know something about a specific child so
  it's a different matter.
|#
(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

    (defmem (gender c) (categorical-vw2 (vector 1/2 1/2) (vector "boy" "girl")))
    
    (define child1 (gender 1))
    (define child2 (gender 2))

   (define num_boys (+ (if (eq? child1  "boy") 1 0) (if (eq? child2 "boy") 1 0)))
    
    ;; "The eldest is a boy"
    (observe/fail (eq? child1 "boy"))

    (list child1
          child2
          (= num_boys 2)
          (= num_boys 1)          
    )
   )
)

(displayln "Model 2")
(show-marginals (model2)
                (list  "child1"
                       "child2"
                       "two boys"
                       "one boy and one girl"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


