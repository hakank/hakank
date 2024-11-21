#| 

  Log gamma dist in Racket/Gamble 

  From Handbook on probability distributions
  page 69ff

  variable : d
  3.579542742682492: 0.00010000000000000938
  3.14221933945418: 0.00010000000000000938
  3.7449344300425147: 0.00010000000000000938
  2.29476830402289: 0.00010000000000000938
  2.895129612380448: 0.00010000000000000938
  ...
  2.2665183139547223: 0.00010000000000000938
  2.7685286432899647: 0.00010000000000000938
  3.0713565927073496: 0.00010000000000000938
  3.5983487462040196: 0.00010000000000000938
  2.588372651013364: 0.00010000000000000938
  mean: 2.9224993849486753
  Min: -0.3764145635248637 Mean: 2.9234476449541615 Max: 4.552781313509644 Variance: 0.3873753540527263 Stddev: 0.6223948538128559
  HPD interval (0.84): 2.127081094247921..3.8114525645881123

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

   (define d (log_gamma 3 2 1))
   (list d
         )
   )
)

(show-marginals (model)
              (list  "d"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


