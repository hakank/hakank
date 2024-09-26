#| 

  Expected breakup length in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/204
  """
  A stick is broken into 3 pieces, by randomly choosing two points along 
  its unit length, and cutting it. What is the expected length of the middle part?

  Answer: 1/3
  """

  * beta 1 1
  var : middle
  0.6223298639780634: 1.0000000000019164e-5
  0.14554458217557958: 1.0000000000019164e-5
  0.13824796294370978: 1.0000000000019164e-5
  0.05925135703718476: 1.0000000000019164e-5
  0.31286886485429477: 1.0000000000019164e-5
  ...
  0.2841244011869483: 1.0000000000019164e-5
  0.7235112566994788: 1.0000000000019164e-5
  0.1748219242312597: 1.0000000000019164e-5
  0.6989607307729907: 1.0000000000019164e-5
  0.4037994373920849: 1.0000000000019164e-5
  mean: 0.3338568659931361

  * dirichlet
  var : middle
  0.3451824115977142: 0.00010000000000000938
  0.3326304863006602: 0.00010000000000000938
  0.33048235123873587: 0.00010000000000000938
  0.33029325811096083: 0.00010000000000000938
  0.3302235242036449: 0.00010000000000000938
  ...
  0.3513335654950063: 0.00010000000000000938
  0.32788754714134577: 0.00010000000000000938
  0.32559038505895116: 0.00010000000000000938
  0.3450474270797351: 0.00010000000000000938
  0.34355285427277715: 0.00010000000000000938
  mean: 0.33259741092103323



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

   (define p1 (beta 1 1))
   (define p2 (beta 1 1))
   (define middle (abs (- p1 p2)))

   (list middle
         )
   
   )
)

(displayln "Using beta 1 1")
(show-marginals (model)
                (list  "middle"
                     )
                #:num-samples 10000
                #:truncate-output 5
                )

;
; Using Dirichlet
;
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;
   ; Note: We have to tweak the values of alpha.
   ; and alpha of (vector 1 1 1) gives a too low value ~ 0.27
   ;
   (define stick (sort (vector->list (dirichlet (vector 100 100 100)))  <))
   
   (define middle (list-ref stick 1))

   (list middle
         )
   
   )
)

(displayln "\nUsing dirichlet (100 100 100)")
(show-marginals (model2)
                (list  "middle"
                     )
                #:num-samples 10000
                #:truncate-output 5
                )


