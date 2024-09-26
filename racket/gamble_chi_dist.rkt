#| 

  Chi dist (generating samples) in Racket.Gamble 

  From Handbook on probability distributions
  page 77ff

  var : g
  1.1338864799503106: 0.00010000000000000938
  0.9651389846837699: 0.00010000000000000938
  1.468249058670818: 0.00010000000000000938
  0.1807265232144507: 0.00010000000000000938
  0.6161546921470026: 0.00010000000000000938
  ...
  0.760225673056029: 0.00010000000000000938
  1.3074717869845667: 0.00010000000000000938
  1.4233824002091275: 0.00010000000000000938
  1.6681596419851523: 0.00010000000000000938
  1.1073482165920228: 0.00010000000000000938
  mean: 1.252860887501989
  Min: 0.00835227488546354 Mean: 1.2463897606917589 Max: 4.598363438237772 Variance: 0.4361400642542122 Stddev: 0.6604090128505306



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

   (define k 2)
   (define g (chi_dist k))

   (list g
         )
   
   )
)

(show-marginals (model)
              (list  "g"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


