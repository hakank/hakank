#| 

  Robot localization in Racket.Gamble 

  From the SPPL model robot-localization.pynb
  The example just shows the model.

  For this model we observe that x is in 1..3

  var : param
  mean: 22.696500000002207
  Credible interval (0.94): 0..41

  var : which
  mean: 0.47733929259466695
  Credible interval (0.94): 0.01777892343188079..0.9063936638482573

  var : x
  mean: 2.022569126170122
  Credible interval (0.94): 1.1009517398300475..2.967856006367458


  This is a port of my WebPPL model robot_localization.wppl.

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

   (define param (random-integer 101))
   (define which (uniform 0 1))

   (define x
     (cond
       [(< which 0.9) (normal (/ param 10) 1)]
       [(< which 0.95) (uniform 0 10)]
       [else 10.0]))
    
    (observe/fail (<= 1.0 x 3.0))
    ; (observe/fail (< x 3.0))

    (list param
          which
          x
          )

   )
)

(show-marginals (model)
                (list  "param"
                       "which"
                       "x"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.94
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


