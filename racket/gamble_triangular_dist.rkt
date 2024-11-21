#| 

  Triangular distribution in Racket/Gamble 

  From Mathematica TriangularDistribution
  """
  An executive is given an account of historical seasonal demands for a product in 
  millions of units. The minimum, maximum, and most likely demands are 1, 1.4, 
  and 1.25, respectively. Find the expected demand [...] using TriangularDistribution:

  td = TriangularDistribution[{1, 1.4}, 1.25];
  Mean[td]
  -> 1.21667
  """

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

   (define min-val 1)
   (define max-val 1.4)
   (define mode 1.25)

   (define d (triangular_dist min-val max-val mode))

   (list d
    )

   )
)

(displayln "Model 1")
(show-marginals (model)
              (list  "d"
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


#|
  Another example
  From https://www.statology.org/triangular-distribution
  """
  Suppose a restaurant estimates that their total sales for the upcoming 
  week will be a minimum of $10,000, a maximum of $30,000, and most likely $25,000.

  What is the probability that the restaurant makes less than [or equal to] $20,000 total sales?
  ...
  0.333

  What is the mean expected sales for the restaurant?
  ...
  The mean expected sales is 21,667.

  """

  variable : d
  mean: 21638.978200181893

  variable : p
  mean: 0.3345000000000195

  sales <= 20000: 0.3333333333333333
  mean: 21666.666666666668

|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define min-val 10000)
   (define max-val 30000)
   (define mode    25000)

   (define d (triangular_dist min-val max-val mode))
   (define p (< d 20000))
   (list d
         p
    )

   )
)
(displayln "Model 2")
(show-marginals (model2)
                (list  "d"
                       "p"
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

(newline)
(displayln (format "sales <= 20000: ~a" (triangular_dist_cdf 10000.0 30000 25000 20000)))
(displayln (format "mean: ~a" (triangular_dist_mean 10000.0 30000 25000)))
