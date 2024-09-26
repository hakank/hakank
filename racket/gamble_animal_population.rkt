#| 

  Animal population in Racket.Gamble 

  From Statistics101 (Resampling Stats)
  File scalarizeExample.txt (edited)
  """
  From: http://www.resample.com/content/software/standalone/userguide/09-commands2.pdf

  Suppose we have a population that is 30% voles, 50% mice and 20% rabbits. 
  In a sample of 10 animals, what is the probability of observing 
  exactly 4 voles, 5 mice and 1 rabbit?
  -> 0.062
  """

  Model1 and model2 use enumerate and give the exact probability: 5103/80000 (0.0637875).
  Model 2 (using categorical-vw2) is perhaps a little faster then model 1: 1.940 vs 2.015s.

  Model3 is a resampling version (importance-sampler 10000 samples) and is the fastest: 0.358s.
  (100000 samples takes about 3.7s.)
  

  * Model1 

  var : voles
  mean: 3 (3.0)

  var : mice
  mean: 5 (5.0)

  var : rabbits
  mean: 2 (2.0)

  var : p
  mean: 5103/80000 (0.0637875)

  cpu time: 1940 real time: 1944 gc time: 119

  * Model 2

  var : voles
  mean: 3 (3.0)

  var : mice
  mean: 5 (5.0)

  var : rabbits
  mean: 2 (2.0)

  var : p
  mean: 5103/80000 (0.0637875)

  cpu time: 561 real time: 563 gc time: 71

  * Model 3 resampling (importance-sampling 10000 samples

  Model3
  var : voles
  mean: 2.9917000000000007

  var : mice
  mean: 4.997800000000001

  var : rabbits
  mean: 2.0105000000000004

  var : p
  mean: 0.06330000000000001

  cpu time: 358 real time: 358 gc time: 1


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define animals_a (append
                   (ones-list 30 "voles")
                   (ones-list 50 "mice")
                   (ones-list 20 "rabbits")))

(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define animals (resample 10 animals_a))
   (define voles (count-occurrences-eq "voles" animals))
   (define mice (count-occurrences-eq "mice" animals))
   (define rabbits (count-occurrences-eq "rabbits" animals))

   (define p (and (= voles 4) (= mice 5) (= rabbits 1)))
   
   (list voles
         mice
         rabbits
         p
         )
   
   )
)

(displayln "\nModel1")
(time (show-marginals (model1)
                (list  "voles"
                       "mice"
                       "rabbits"
                       "p"
                     )
                    #:num-samples 1000
                    ; #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ))


(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ; (define animals (repeat (lambda () (categorical-vw2 (vector 30 50 20) (vector "voles" "mice" "rabbits"))) 10))
   (define animals (resample 10 animals_a))   
   (define voles (count-occurrences-eq "voles" animals))
   (define mice (count-occurrences-eq "mice" animals))
   (define rabbits (count-occurrences-eq "rabbits" animals))

   (define p (and (= voles 4) (= mice 5) (= rabbits 1)))
   
   (list voles
         mice
         rabbits
         p
         )
   
   )
)

(displayln "\nModel2")
(time (show-marginals (model2)
                (list  "voles"
                       "mice"
                       "rabbits"
                       "p"
                     )
                    #:num-samples 1000
                    ; #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ))


(define (model3)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define animals (repeat (lambda () (categorical-vw2 (vector 30 50 20) (vector "voles" "mice" "rabbits"))) 10))
   (define voles (count-occurrences-eq "voles" animals))
   (define mice (count-occurrences-eq "mice" animals))
   (define rabbits (count-occurrences-eq "rabbits" animals))

   (define p (and (= voles 4) (= mice 5) (= rabbits 1)))
   
   (list voles
         mice
         rabbits
         p
         )
   
   )
)

(displayln "\nModel3")
(time (show-marginals (model3)
                (list  "voles"
                       "mice"
                       "rabbits"
                       "p"
                     )
                    #:num-samples 10000
                    ; #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ))


