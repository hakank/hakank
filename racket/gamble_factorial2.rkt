#| 

  Factorial in Racket/Gamble 

  From BLOG example/factorial.blog

  This is a port of my WebPPL model factorial2.wppl

  var : f 3
  6: 0.9999999999999994
  mean: 5.9999999999999964

  var : X
  1: 0.07899999999999996
  8: 0.07599999999999997
  15: 0.07499999999999997
  12: 0.07299999999999997
  6: 0.07099999999999997
  ...
  7: 0.06199999999999997
  10: 0.05999999999999997
  14: 0.05499999999999998
  4: 0.05399999999999998
  2: 0.05299999999999998
  mean: 8.053999999999998

  var : f X
  1: 0.07899999999999996
  40320: 0.07599999999999997
  1307674368000: 0.07499999999999997
  479001600: 0.07299999999999997
  720: 0.07099999999999997
  ...
  5040: 0.06199999999999997
  3628800: 0.05999999999999997
  87178291200: 0.05499999999999998
  24: 0.05399999999999998
  2: 0.05299999999999998
  mean: 103344163181.73497

  var : Y
  -1: 0.32899999999999985
  362880: 0.05399999999999998
  1: 0.05199999999999998
  6227020800: 0.05199999999999998
  40320: 0.04999999999999998
  ...
  120: 0.04399999999999998
  3628800: 0.042999999999999976
  2: 0.03599999999999998
  24: 0.03499999999999999
  87178291200: 0.029999999999999985
  mean: 67038846838.25898

  var : X Y
  (9 362880): 0.05399999999999998
  (13 6227020800): 0.05199999999999998
  (1 1): 0.05199999999999998
  (8 40320): 0.04999999999999998
  (15 1307674368000): 0.04899999999999998
  ...
  (7 -1): 0.01799999999999999
  (13 -1): 0.01799999999999999
  (10 -1): 0.016999999999999994
  (2 -1): 0.016999999999999994
  (9 -1): 0.015999999999999993


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

   (define (f k)
     (if (<= k 1)
         1
         (* k (f (sub1 k)))))
   
   (define X (add1 (random-integer 15)))
    
   (define W (uniform 0.0 0.5))
    
   (define Y (categorical-vw2 (vector 0.5 W) (vector (f X) -1)))

   (list (f 3)
         X
         (f X)
         Y
         (list X Y)
    )

   )
)

(show-marginals (model)
                (list  "f 3"
                       "X"
                       "f X"
                       "Y"
                       "X Y"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


