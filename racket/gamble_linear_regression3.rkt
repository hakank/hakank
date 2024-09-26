#| 

  Linear regression in Racket.Gamble 

  Port of PyMC3 first linear regression model in
  https://docs.pymc.io/pymc-examples/examples/generalized_linear_models/GLM.html

  Note: The PyMC3 model(s) in that page use  GLM.from_formula which is not available
  in Gamble, so this is not a good port at all.

  Not very good, but probably acceptable. 

  (true_intercept: 1 true_slope: 2)

  * Using mh-sampler with 100000 samples (18s)

  var : intercept
  mean: 1.1648154050865138

  var : slope
  mean: 1.7730248285134373

  var : sigma
  mean: 0.6007923354928814


  * mh-sampler #:transition (slice) (took much longer time 1min47s)
  var : intercept
  mean: 1.012880580344744

  var : slope
  mean: 2.228868897205172

  var : sigma
  mean: 0.577457434112158


  * Importance sampler, 100000 samples (11s)

  var : intercept
  mean: 0.9342914193806203

  var : slope
  mean: 1.8843994788343865

  var : sigma
  mean: 0.4959030988683972



  This is a port of my WebPPL model linear_regression3.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;; Generate some random values.
;; We are trying to detect true intercept and true slope
(define n 50)
(define true_intercept 1)
(define true_slope 2)
(show2 "true_intercept:" true_intercept "true_slope:" true_slope)

(define x (range 0.0 1 (/ 1 n)))
(define y (for/list ([i n]) (+ true_intercept (* (list-ref x i) true_slope) (normal 0 0.5))))
; (show "x" x)
; (show "y" y)

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice)
   
   (define n (length x))
   (define intercept (normal 0 20))
   (define slope (normal 0 10))
   (define sigma (uniform 0 2))
   
   (for ([i n])
     (observe-sample (normal-dist (+ intercept (* slope (list-ref x i))) sigma) (list-ref y i)))
     
   (list intercept
         slope
         sigma
         )
   
   )
)

(show-marginals (model)
                (list  "intercept"
                       "slope"
                       "sigma"
                       )
                #:num-samples 100000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


