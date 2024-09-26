#| 

  Random ratio in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/112
  """
  p and q are two points chosen at random between 0 & 1. What is the probability 
  that the ratio p/q lies between 1 & 2?

  Answer: 0.25
  """

  var : ratio
  0.2775456925119042: 1.0000000000019164e-5
  1.539017384756392: 1.0000000000019164e-5
  0.05357722249849899: 1.0000000000019164e-5
  1.4396925307264294: 1.0000000000019164e-5
  0.1360127700838246: 1.0000000000019164e-5
  ...
  0.08285097792598105: 1.0000000000019164e-5
  4.85883330816453: 1.0000000000019164e-5
  0.5226776219869175: 1.0000000000019164e-5
  8.529872006488077: 1.0000000000019164e-5
  2.680675881825706: 1.0000000000019164e-5
  mean: 5.617143678830062
  Credible interval (0.94): 2.790650600008444e-5..8.136926901811888

  var : prob
  #f: 0.7501500000022776
  #t: 0.24985000000038862
  mean: 0.24985000000038862


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define p (beta 1 1))
   (define q (beta 1 1))

   (define ratio (/ p q))

   (define prob (and (>= ratio 1) (<= ratio 2)))
   
   (list ratio
         prob)
   
   )
)

(show-marginals (model)
                (list  "ratio"
                       "prob"
                     )
                    #:num-samples 100000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.94
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


