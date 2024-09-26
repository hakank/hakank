#| 

  Chi squared distribution (generating samples) in Racket.Gamble 

  From Handbook on probability distributions
  page 75ff

  var : g
  5.762252545255885: 0.00010000000000000938
  1.8461983714798935: 0.00010000000000000938
  0.5371151571829118: 0.00010000000000000938
  3.795758418638744: 0.00010000000000000938
  0.48448354780155517: 0.00010000000000000938
  ...
  3.3577531436688397: 0.00010000000000000938
  1.3282168541165693: 0.00010000000000000938
  3.634032702104628: 0.00010000000000000938
  3.5201443764054856: 0.00010000000000000938
  2.991953086145498: 0.00010000000000000938
  mean: 4.044014403507719
  Min: 0.02731194441308551 Mean: 4.03173056777405 Max: 25.954795306119806 Variance: 8.0951813813612 Stddev: 2.8452032232094076


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

   (define k 4)
   (define g (chi_squared_dist k))

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


