#| 

  Piecewise transformation in Racket.Gamble 

  Based on the SPPL model piecewise_transformation.pynb

  var : x
  mean: -0.0371095329658942
  Credible interval (0.94): -3.518785513041169..3.9654556451226304

  var : z
  mean: 4.417255407268985
  Credible interval (0.94): -4.060671507308772..20.499071183757298

  This is a port of my WebPPL model piecewise_transformation.wppl 

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

   (define x (normal 0 2))
                  
   (define z (if (< x 1)
                 ; x < 1 ? Math.pow(-x,3)+Math.pow(x,2)+6*x : -5*Math.sqrt(x)+11
                 (+ (expt (- x) 3) (expt x 2) (* 6 x))
                 (+ (* (- 5) (sqrt x)) 11)))
    
   (list x
         z
    )
   )
)

(show-marginals (model)
                (list  "x"
                       "z"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.94
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


