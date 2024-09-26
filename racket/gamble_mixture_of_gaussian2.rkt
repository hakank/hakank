#| 

  Mixture of gaussian  in Racket.Gamble 

  From BLOG examples/mixture-of-gaussian-full.blog

  var : a
  mean: -0.4776217451252865
  Credible interval (0.84): -1.7543146107464247..0.9400301600201211

  var : b
  mean: 0.008095820436750183
  Credible interval (0.84): -0.5690680766159673..1.138741991144205

  var : p
  mean: 0.30410740737422937
  Credible interval (0.84): 1.9941323876522204e-6..0.706192209372804

  var : min a b
  mean: -0.8279503230395782
  Credible interval (0.84): -1.93..0.44



  This is a port of my WebPPL model mixture_of_gaussian2.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(require racket/set) ; for set-intersect

(define (model)
  (; enumerate ; #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define p (beta 0.5 1))
    
   (define (z i) (bernoulli p))
   
   (define a (normal -1 1))
   (define b (normal -1 1))
   
   (define (x i)
     (if (= (z i) 1)
         (normal-dist a 1.0)
         (normal-dist b 1.0)))
   
   (observe-sample (x 0) 0.2)
   (observe-sample (x 1) 1.0)
   (observe-sample (x 3) 0.5)
   (observe-sample (x 4) 0.6)
   
   (list a
         b
         p
         (roundf (min a b) 0.01)
         )
   

   )
)

(show-marginals (model)
                (list  "a"
                       "b"
                       "p"
                       "min a b"
                       
                       )
                #:num-samples 10000
                #:truncate-output 3
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


