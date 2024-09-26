#| 

  Stick to Triangle in Racket.Gamble 

  From https://brainstellar.com/puzzles/probability/102
  """
  A stick is broken into 3 parts, by choosing 2 points randomly 
  along its length. With what probability can it form a triangle?
  """

  var : p
  #f: 0.74947
  #t: 0.25053
  mean: 0.25053

  var : p2
  #f: 0.74947
  #t: 0.25053
  mean: 0.25053


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

   (define n 3)

   ; Split the stick in 3 random pieces
   (define t (dirichlet (vector 1 1 1)))
   (define a (vector-ref t 0))
   (define b (vector-ref t 1))
   (define c (vector-ref t 2))

   ; Triangle inequality
   (define p (and (<= a (+ b c))
                  (<= b (+ a c))
                  (<= c (+ a b))))

   ; All pieces < 0.5
   (define p2 (andmap (lambda (v) (< v 0.5)) (vector->list t)))
   
   (list p
         p2
         )
   )
)

(show-marginals (model)
                (list  "p"
                       "p2"
                       )
                #:num-samples 1000000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


