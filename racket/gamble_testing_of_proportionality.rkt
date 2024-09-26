#| 

  Testing for Proportionality in Racket.Gamble 

  From A First Course in Mathematical Modeling, 4th edition,
  page 2:

  This is a simple spring-mass system experiment with mass 
  and the elongment:

    mass =  (50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550)
    elong = (1.0, 1.875, 2.75, 3.25, 4.375, 4.875, 5.675, 6.5, 7.25, 8.0, 8.75)

  In the book, the slope is calculated as 0.0163 (with an intercept of 0).

  Here we have two models, without and with intercept.

  * use_intercept: #f
  var : slope
  mean: 0.01645071414267478

  var : intercept
  mean: 0 (0.0)

  * use_intercept: #t
  var : slope
  mean: 0.01172261419393049

  var : intercept
  mean: 1.7152751976046199

  This is a port of my WebPPL model testing_of_proportionality.wppl.
 
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define mass  '( 50   100  150  200   250   300   350 400  450 500  550))
(define elong '(1.0 1.875 2.75 3.25 4.375 4.875 5.675 6.5 7.25 8.0 8.75))

(define (model [use_intercept #f])
  (show "* use_intercept" use_intercept)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define n (length mass))
        
   (define slope (normal 2 1))
   (define intercept (if use_intercept (normal 5 1) 0))
        
   (for ([i n])
     (observe-sample (normal-dist (+ intercept (* (list-ref mass i) slope)) 1) (list-ref elong i)))
        
   (list slope
         intercept
         )
   
   )
)

(for ([use_intercept '(#f #t)])
      (show-marginals (model use_intercept)
                (list  "slope"
                       "intercept"
                       )
                #:num-samples 100000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
      )


