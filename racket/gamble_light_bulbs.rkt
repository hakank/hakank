#| 

  Light bulbs in Racket.Gamble 

  From my WebPPL model
  """
  Today I changed the last of two bulbs which were supposed to hold for 
  2000h (they are Halogen bulbs). The first went out some days ago, 
  say 25h ago.

  What is the probability of this to happen if we assume that the 
  the time for light bulbs are an Exponential distribution?

  Mathematica code:
    Probability(Abs(a - b) <= c, (a, b) ->
      ProductDistribution(ExponentialDistribution(p), 
      ExponentialDistribution(p)))

  1 - E^(-c p) (if c > 0)
  
  -> 0.0124222
  """

  Here's the summary of the three models: Exponential, Gaussian, and Poisson,
  where p is the probability that both bulbs went out in the range of 25h.
  I guess that the exponential model is the most realistic of these.

  * Exponential model *

  var : b1
  mean: 2062.1722507835366
  Min: 1.3444052434230562 Mean: 2094.180572718282 Max: 14588.553465357509 Variance: 4214750.623796757 Stddev: 2052.9857826582133

  var : b2
  mean: 2007.8400305921227
  Min: 1.6593136555411745 Mean: 2056.7236872857698 Max: 16544.95011997502 Variance: 4484917.003999762 Stddev: 2117.7622633335786

  var : diff
  mean: 2060.987892184262
  Min: 0.30991120602595856 Mean: 2094.0963822419576 Max: 15078.269898166789 Variance: 4221393.5890313955 Stddev: 2054.60302468175

  var : p
  mean: 0.007999999999999995
  Min: 0 Mean: 0.01 Max: 1 Variance: 0.0099 Stddev: 0.099498743710662


  Note that the mean difference between b1 and b2 is about the same as b1 and b2. This
  is the property of exponential distribution.


  * Gaussian model *

  var : b1
  mean: 1997.5458263097457
  Min: 1621.9371706270804 Mean: 2008.4815097383191 Max: 2286.416363718943 Variance: 9657.626687118514 Stddev: 98.27322467039797

  var : b2
  mean: 1999.8032581675632
  Min: 1705.720622522694 Mean: 1994.819290309575 Max: 2290.785419252358 Variance: 9845.224629239334 Stddev: 99.22310531947352

  var : diff
  mean: 114.90316659543203
  Min: 0.42984668887038424 Mean: 111.30555850138079 Max: 516.0181049171015 Variance: 7605.913476476219 Stddev: 87.21188838957805

  var : p
  mean: 0.1400000000000001
  Min: 0 Mean: 0.159 Max: 1 Variance: 0.133719 Stddev: 0.36567608617463626



  * Poisson model *
  var : b1
  mean: 1998.6750000000004
  Min: 1866 Mean: 2000.204 Max: 2161 Variance: 2037.900384 Stddev: 45.14311003907462

  var : b2
  mean: 1999.7849999999983
  Min: 1881 Mean: 1999.614 Max: 2136 Variance: 2004.853004 Stddev: 44.77558490963574

  var : diff
  mean: 51.71399999999998
  Min: 0 Mean: 50.61 Max: 219 Variance: 1408.1539 Stddev: 37.52537674694286

  var : p
  mean: 0.2910000000000002
  Min: 0 Mean: 0.314 Max: 1 Variance: 0.215404 Stddev: 0.46411636471902173


  This is a port of my WebPPL model light_bulbs.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


;; Exponential
(define (model-exp)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define lambda_ 2000) ; Note Webppl: 1/2000
   (define b1 (exponential lambda_))
   (define b2 (exponential lambda_))
   (define diff (abs (- b1 b2)))
   (define p (<= diff 25))

   (list b1
         b2
         diff
         p
    )

   )
)

(displayln "* Exponential model *")
(show-marginals (model-exp)
                (list  "b1"
                       "b2"
                       "diff"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


;;  Gaussian distribution: 2000h, standard deviation 100h
(define (model-gaussian)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define mean 2000)
   (define stdev 100)
   (define b1 (normal mean stdev))
   (define b2 (normal mean stdev))
   (define diff (abs (- b1 b2)))
   (define p (<= diff 25))

   (list b1
         b2
         diff
         p
    )

   )
)

(displayln "* Gaussian model *")
(show-marginals (model-gaussian)
                (list  "b1"
                       "b2"
                       "diff"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


;; Poisson distribution: 1/2000h 
(define (model-poisson)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define lambda_ 2000)
   (define b1 (poisson lambda_))
   (define b2 (poisson lambda_))
   (define diff (abs (- b1 b2)))
   (define p (<= diff 25))

   (list b1
         b2
         diff
         p
    )

   )
)

(displayln "* Poisson model *")
(show-marginals (model-poisson)
                (list  "b1"
                       "b2"
                       "diff"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

