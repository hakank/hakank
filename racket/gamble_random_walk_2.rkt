#| 

  Random walk 2D in Racket.Gamble 

  Cf random_walk_1.wppl.
   
  This is a 2D version of a random walk. 
  The stop criteria is when we're back to the start position (0,0)
  (or reaching a limit).

  This example is from 
  Fredrik Dahlqvist and Alexandra Silva
  "Semantics of Probabilistic Programming: A Gentle Introduction"

  var : length arr
  mean: 360.2090000000025

  var : p
  mean: 0.6805999999999998

  This is a port of my WebPPL model random_walk_2.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (model a b limit)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define (step u v)
     (let* ([x (bernoulli 1/2)]
            [y (bernoulli 1/2)]
            [a (+ u (- x y))]
            [b (+ v (- (+ x y) 1))])
       (list a b)))
   
   (define (walk arr)
     (let ([len (length arr)])
       ;; We have to create a limit, otherwise it might going to infinity...
       (if (> len limit)
           arr
           (let* ([uv (last arr)]
                  [xy (step (first uv) (second uv))])
             (if (and (= (first xy) 0) (= (second xy) 0))
                 arr
                 (walk (append arr (list xy))))))
       ))
   
   (define arr (walk (list (list 0 0))))
   (define p (<= (length arr) limit))
   
   (list (length arr)
         p
         )
   
   )
  
)


(show-marginals (model 0 0 1000)
                (list  "length arr"
                       "p"
                     )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
