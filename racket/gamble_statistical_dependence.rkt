#| 

  Statistical dependence in Racket.Gamble 

  From https://mhtess.github.io/bdappl/chapters/05-patterns.html

  Observation:: ()
  var : a
  mean: 0.5499999999999999

  var : b
  mean: 0.4999999999999999

  var : c
  mean: 0.49999999999999994


  Observation:: #f
  var : a
  mean: 0.2

  var : b
  mean: 0 (0.0)

  var : c
  mean: 0.5


  Observation:: #t
  var : a
  mean: 0.8999999999999999

  var : b
  mean: 0.9999999999999998

  var : c
  mean: 0.4999999999999999


  This is a port of my WebPPL model statistical_dependence.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model [observation '()])
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define c (flip 0.5))
   (define b (flip 0.5))
   (define a (if b (flip 0.9) (flip 0.2)))

   (when (not (empty? observation))
     (observe/fail (eq? b observation))
     )
   
   (list a
         b
         c
         )

   )
)

(for ([observation '( () #f #t)])
      (show "Observation:" observation)
      (show-marginals (model observation)
                      (list  "a" "b" "c")
                      #:skip-marginals? #t)
      (newline))
