#| 

  Gaussian (normal) dist in Racket/Gamble 

  From Handbook on probability distributions
  page 47ff


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define g1 (normal 0 1)) ; Built-in version
   (define g2 (gaussian01)) ; This version

   (define mean 100)
   (define std 10)
   (define g3 (normal mean std))       ; Built-in version
   (define g4 (gaussian_dist mean std)) ;  This version

   (list g1
         g2
         g3
         g4
    )
   )
)

(show-marginals (model)
                (list  "g1"
                       "g2"
                       "g3"
                       "g4"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


