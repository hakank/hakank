#| 

  Pi in Racket.Gamble 

  var : pi
  4: 0.78576
  0: 0.21424
  mean: 3.14304

 
  This is a port of my WebPPL model pi.wppl

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
   
   (define x (uniform -1 1))
   (define y (uniform -1 1))
   (define pi (* 4 (if (< (+ (expt x 2) (expt y 2)) 1) 1 0)))
   
   (list pi
         )
    
   )
)

(show-marginals (model)
                (list  "pi"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


