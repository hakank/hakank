#| 

  Kumaraswamy distribution in Racket/Gamble 

  From Handbook on probability distributions
  """
  Since the quantile function is explicit
     F^-1(u) = (1 - (1 - u)^(1/b))^(1/a)
  an inversion function method F^-1(u) with u uniformly distributed is easily computable.
  """

  variable : g
  0.7214583497043321: 0.00010000000000000938
  0.6650505628305692: 0.00010000000000000938
  0.87782636540572: 0.00010000000000000938
  0.9749782867050242: 0.00010000000000000938
  0.8779936876533385: 0.00010000000000000938
  ...
  0.8087496307909373: 0.00010000000000000938
  0.7868636797818431: 0.00010000000000000938
  0.804400717351552: 0.00010000000000000938
  0.6845421579697586: 0.00010000000000000938
  0.656973121329324: 0.00010000000000000938
  mean: 0.759797676635185

  kumaraswamy_dist_mean 5 2: 0.7575757575757573

  See gamble_distributions.rkt and gamble_distributions_test.rkt for more tests.

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

   (define a 5)
   (define b 2)
   (define g (kumaraswamy_dist a b))

   (list a
         b
         g
    )
   )
)

(show-marginals (model)
                (list  "a"
                       "b"
                       "g"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


(show "kumaraswamy_dist_mean 5 2" (kumaraswamy_dist_mean 5 2))
