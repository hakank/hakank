#| 

  Lions, tigers and bears in Racket Gamble.

  From Allen Downey
  https://twitter.com/AllenDowney/status/1063460117029535746
  """
  Today's Bayesian problem of the week: Suppose we visit a wild animal preserve where we 
  know that the only animals are lions and tigers and bears, but we don't know how 
  many of each there are.

  During the tour, we see 3 lions, 2 tigers, and 1 bear. Assuming that every animal had an equal 
  chance to appear in our sample, estimate the prevalence of each species. 
 
  What is the probability that the next animal we see is a bear?
  """

  Also see: https://towardsdatascience.com/estimating-probabilities-with-bayesian-modeling-in-python-7144be007815

  This version uses dirichlet for the prior.

  * prior: dirichlet (3 2 1)
  var : o 6 == lion
  #t: 0.5090000000000003
  #f: 0.4910000000000004
  mean: 0.5090000000000003

  var : o 6 == tiger
  #f: 0.6640000000000005
  #t: 0.33600000000000024
  mean: 0.33600000000000024

  var : o 6 == bear
  #f: 0.8450000000000006
  #t: 0.1550000000000001
  mean: 0.1550000000000001

  var : prob lion
  mean: 0.5029681677652637

  var : prob tiger
  mean: 0.3345225819910033

  var : prob bear
  mean: 0.16250925024373172

  * prior: dirichlet (1 1 1)

  var : o 6 == lion
  #f: 0.5660000000000004
  #t: 0.43400000000000033
  mean: 0.43400000000000033

  var : o 6 == tiger
  #f: 0.6750000000000005
  #t: 0.32500000000000023
  mean: 0.32500000000000023

  var : o 6 == bear
  #f: 0.7590000000000006
  #t: 0.2410000000000002
  mean: 0.2410000000000002

  var : prob lion
  mean: 0.44743995453965824

  var : prob tiger
  mean: 0.3319865849449831

  var : prob bear
  mean: 0.22057346051535776



  This is a port of my WebPPL model lions_tigers_and_bears2.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  
  (; enumerate ; cannot enumerate irichlet-dist
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ;; The animals.
   (define lion 0)
   (define tiger 1)
   (define bear 2)
   
   ;; Prior
   ;; The Dirichlet distribution ensures that the sum of probabilities is 1
   ;; i.e. we don't have to ensure this via some specific constraint.
   (define v (vector 1 1 1))
   (define x (dirichlet v))
   
   ;; The probabilities to calculate ("aliased" for simplicity)
   (define probLion  (vector-ref x 0))
   (define probTiger (vector-ref x 1))
   (define probBear  (vector-ref x 2))
   
   ;; Posterior: What is the probability of an i'th lion, tiger, and bear given the observations?   
   (define o (mem (lambda (i) (categorical-vw2 x (vector lion tiger bear)))))
   

   ;; It shouldn't matter in what order we see the different animals.       
   (observe/fail (eq? (o 0) lion))
   (observe/fail (eq? (o 1) lion))
   (observe/fail (eq? (o 2) lion))
   (observe/fail (eq? (o 3) tiger))
   (observe/fail (eq? (o 4) tiger))
   (observe/fail (eq? (o 5) bear))
   
   (list 
     (eq? (o 6) lion)  ; probability that we see a lion in observation (o 6) (the 7'th observation)
     (eq? (o 6) tiger) ;                           tiger
     (eq? (o 6) bear)  ;                           bear
     probLion          ; probability of lion
     probTiger         ;                tiger
     probBear          ;                bear     
     )
   
   )
  )

(show-marginals (model)
                (list "o 6 == lion"
                      "o 6 == tiger"
                      "o 6 == bear"
                      "prob lion"
                      "prob tiger"
                      "prob bear"
                      )
                #:num-samples 1000
                #:truncate-output 1
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                )


