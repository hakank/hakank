#| 

  How many sons? in Racket.Gamble 

  From Ruma Falk:
  "Understanding Probability and Statistics - A Book of Problems"
  page 56

  """
  How many sons?

  Mrs. F is known to be the mother of two. You meet her in town
  with a boy whom she introduces as her son. What is the probability
  that Mrs. F has two sons? One possible is that the probability 
  is 1/2: you now have seen one boy, and there the question is 
  whether the other child is a male. Another possibility is that
  the probability is 1/3: you learned that Mrs. F has 'at least
  one boy', and hence three equiprobable family structures are
  possible (BB, BG, GB), of which the target event (BB) is but one.
  Which is the correct answer? Explain.

  (Hint: What are your assuptions about the chance mechanism that
  yielded your observation of Mrs. F hand her son?)
  """

  This problem (with a slighly different wording) was also in
  Maya Bar-Hillel & Ruma Falk: "Some teasers concerning conditional
  probabilities" (Cognition 11, 1982, page 109-122).


  Here we show these two different approaches:
  - model1: p(two_sons) = 1/2
  - model2: p(two_sons) = 1/3

  As commented in the hint (and the comments at page 177), the 
  assumptions regarding the "chance mechanism" - i.e. how we 
  know what - matters.

  This is a port of my WebPPL model how_many_sons.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


#|
  Model 1

  In this model we know that the "first child" is a son.

  var : child1
  son: 1 (1.0)

  var : child2
  son: 1/2 (0.5)
  daughter: 1/2 (0.5)

  var : twoSons
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

|#   
(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define g (list "son" "daughter"))
    
   (defmem (gender i) (uniform-draw g))

   (define child1 (gender 0))
   (define child2 (gender 1))

   (observe/fail (eq? child1 "son"))
    
   (list child1
         child2
         (eq? child2 "son")
         )
   
   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list  "child1"
                       "child2"
                       "twoSons"
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
  Model 2

  In this model we only know that one of the children
  - but not which of them - is a son.

  var : numSons
  1: 2/3 (0.6666666666666666)
  2: 1/3 (0.3333333333333333)
  mean: 4/3 (1.3333333333333333)

  var : children
  (son daughter): 1/3 (0.3333333333333333)
  (daughter son): 1/3 (0.3333333333333333)
  (son son): 1/3 (0.3333333333333333)

  var : twoSons
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)

|#   
(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define g (list "son" "daughter"))
    
   (defmem (gender i) (uniform-draw g))
   
   ;; The gender of the two children
   (define children (for/list ([i 2]) (gender i)))
    
   ;; How many of the childrens are sons?
   (define numSons (for/sum ([c children]) (boolean->integer (eq? c "son"))))

   ;; We know that at least one of the children is a son
   (observe/fail (>= numSons 1))
   
   (list numSons
         children
         (= numSons 2)
         )
   
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "numSons"
                       "children"
                       "twoSons")
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

