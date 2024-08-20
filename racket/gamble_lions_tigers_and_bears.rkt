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

  * first model

    Note that the sum of the probabilities does not sums to 1.
    For a fix, see model2 below or lions_tigers_and_bears2.rkt 
    which uses dirichlet as a prior.

  var : o 6 == lion
  #f: 0.5790000000000004
  #t: 0.4210000000000003
  mean: 0.4210000000000003

  var : o 6 == tiger
  #f: 0.6900000000000005
  #t: 0.3100000000000002
  mean: 0.3100000000000002

  var : o 6 == bear
  #f: 0.7310000000000005
  #t: 0.2690000000000002
  mean: 0.2690000000000002

  var : prob lion
  mean: 0.6459284042244606

  var : prob tiger
  mean: 0.5515528949530873

  var : prob bear
  mean: 0.41525959097668974


  * model2

  var : o 6 == lion
  #f: 0.5990000000000004
  #t: 0.4010000000000003
  mean: 0.4010000000000003

  var : o 6 == tiger
  #f: 0.6690000000000005
  #t: 0.33100000000000024
  mean: 0.33100000000000024

  var : o 6 == bear
  #f: 0.7320000000000005
  #t: 0.2680000000000002
  mean: 0.2680000000000002

  var : prob lion
  mean: 0.40639483942759747

  var : prob tiger
  mean: 0.3372575867975058

  var : prob bear
  mean: 0.256347573774896



  This is a port of my WebPPL model lions_tigers_and_bears.wppl

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
   
   ;; Priors
   ; Note: We don't ensure that the sum of these probs sums to 1.
   ; See gamble_lions_tigers_and_bears2.rkt which uses dirichlet for this)
   ; And a variant is shown in model2 below
   (define probLion  (beta 1 1))
   (define probTiger (beta 1 1))
   (define probBear  (beta 1 1))
   
   ;; Posterior: What is the probability of an i'th lion, tiger, and bear given the observations?   
   (define o (mem (lambda (i) (categorical-vw2 (vector probLion probTiger probBear) (vector lion tiger bear)))))
   
   ; Too slow
   ; (observe/fail (< (abs (- 1 (+ probLion probTiger probBear))) 0.1))

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

;; (show-marginals (model)
;;                 (list "o 6 == lion"
;;                       "o 6 == tiger"
;;                       "o 6 == bear"
;;                       "prob lion"
;;                       "prob tiger"
;;                       "prob bear"
;;                       )
;;                 #:num-samples 1000
;;                 #:truncate-output 1
;;                 ; #:skip-marginals? #t
;;                 ; #:credible-interval 0.94
;;                 )

;
; In this version we ensure that probLion + probTiger + probBear = 1
; But - as mentioned above - it's neater to use dirichlet as in lions_tigers_and_bears2.rkt.
(define (model2)
  
  (; enumerate ; cannot enumerate irichlet-dist
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ;; The animals.
   (define lion 0)
   (define tiger 1)
   (define bear 2)
   
   ;; Priors
   (define probLion1  (beta 1 1))
   (define probTiger1 (beta 1 1))
   (define probBear1  (beta 1 1))

   ; Ensure that the sum of the probabilities sums to 1
   (define sum-probs (+ probLion1 probTiger1 probBear1))
   (define probLion  (/ probLion1 sum-probs))
   (define probTiger (/ probTiger1 sum-probs))
   (define probBear  (/ probBear1 sum-probs))
   

   ;; Posterior: What is the probability of an i'th lion, tiger, and bear given the observations?   
   (define o (mem (lambda (i) (categorical-vw2 (vector probLion1 probTiger1 probBear1) (vector lion tiger bear)))))

   
   ; Too slow
   ; (observe/fail (< (abs (- 1 (+ probLion probTiger probBear))) 0.1))

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

(show-marginals (model2)
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


