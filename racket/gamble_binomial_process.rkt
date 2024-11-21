#| 

  BinomialProcess in Racket/Gamble 

  From Mathematica BinomialProcess

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

#|
  From Mathematica BinomialProcess
  """
  A quality assurance inspector randomly selects a series of 10 parts from a 
  manufacturing process that is known to produce 20% bad parts. Find the probability 
  that the inspector gets at most one bad part:

  selectionProcess = BinomialProcess[0.2];
  NProbability[x[10] <= 1, x \[Distributed] selectionProcess]
  -> 
  0.37581
  """

  enumerate is too slow for t=10, so we use importance-sampler instead.

  (binomial_process_cdf 0.2 10 1): 0.37580963840000037

  variable : val
  2: 0.30285300000000004
  1: 0.26803000000000005
  3: 0.20130300000000004
  0: 0.10715900000000002
  4: 0.08772900000000002
  5: 0.026565000000000002
  6: 0.005501000000000001
  7: 0.0007910000000000001
  8: 6.400000000000001e-5
  9: 5.000000000000001e-6
  mean: 2.0004860000000004

  variable : prob
  #f: 0.624811
  #t: 0.37518900000000005
  mean: 0.37518900000000005


|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define p 2/10)
   (define p 0.2)   
   (define t 10)

   (define x (for/list ([i t]) (binomial t p)))

   (define val (last x))
   (define prob (<= val 1))

   (list val
         prob
         )
   )
)

(show "(binomial_process_cdf 0.2 10 1)" (binomial_process_cdf 0.2 10 1))
(newline)
(show-marginals (model)
                (list  "val"
                       "prob"
                       )
                #:num-samples 1000000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


