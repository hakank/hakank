#| 

  Cauchy distribution (generating random samples) in Racket.Gamble 

  From Handbook on probability distributions
  page 86ff.

  var : g1
  mean: 1.9726335206625638
  Min: -2718.5423139534646 Mean: -4.32866669602124 Max: 324.22146836773936 Variance: 12426.517190356579 Stddev: 111.47428936914817

  var : g2
  mean: 1.3215782207394848
  Min: -510.8163012916249 Mean: 2.306554102177584 Max: 665.8204783540888 Variance: 1278.702475371904 Stddev: 35.758949584291535

  var : g3
  mean: 7.575376963335987
  Min: -16689.336258044106 Mean: -15.51672821432913 Max: 413.7927633652218 Variance: 278884.0307828111 Stddev: 528.0947176244155


  Note: Gamble's built-in cauchy distribution should be used. 
        This is just for demonstration purposes.

  See gamble_distributions.rkt for the definitions of PDF, CDF, and quantile, and
  gamble_distributions_test.rkt for comparisons between these and the built-ins.


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

   (define delta 1)
   (define gamma 2)
   (define g1 (cauchy_dist delta gamma))
   (define g2 (cauchy delta gamma)) ; Built-in
   (define g3 (cauchy delta gamma)) ; Checking another run
   
   (list g1
         g2
         g3
         )
   )
)

(show-marginals (model)
                (list  "g1"
                       "g2"
                       "g3"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

