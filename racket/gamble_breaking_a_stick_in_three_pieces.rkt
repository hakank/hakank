#| 

  Breaking a stick in three pieces in Racket.Gamble 

  https://math.stackexchange.com/questions/3872156/uniformly-at-random-break-a-unit-stick-in-two-places-what-is-the-probability-t
  """
  Uniformly at random, break a unit stick in two places. What is the probability that 
  the smallest piece is <= 1/5?

  I was asked this in an interview and wasn't sure how to solve it:

     Consider a stick of length 1
     * Select two points independently and uniformly at random on the stick. Break the stick at these 
       two points, resulting in 3 smaller pieces. What is the probability that the smallest of these 
       pieces is <= 1/5?
  """
  
  Answer: 21/25 = 0.84

  * Model 1
  (define : b1
  mean: 0.3324811729038873

  (define : b2
  mean: 0.33444983171375847

  (define : piece1
  mean: 0.16689308340599007

  (define : piece2
  mean: 0.5000379212116564

  (define : piece3
  mean: 0.33306899538244666

  (define : min_piece
  mean: 0.1107705593464946

  (define : p
  mean: 0.838499999999964

  * Model 2

  var : min_piece
  mean: 0.11149895367457054

  var : p
  mean: 0.8426999999999636


  This is a port of my WebPPL model breaking_a_stick_in_three_pieces.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define b1 (uniform 0 1)) ; break 1
   (define b2 (uniform 0 1)) ; break 2

   (define piece1 (min b1 b2))
   (define piece2 (max b1 b2))
   (define piece3 (- 1 piece1 piece2))
   (observe/fail (>= piece3 0))
   
   (define min_piece (min piece1 piece2 piece3))
   
   (define p (<= min_piece 1/5))

   (list b1
         b2
         piece1
         piece2
         piece3
         min_piece
         p
         )
   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list  "b1"
                       "b2"
                       "piece1"
                       "piece2"
                       "piece3"
                       "min_piece"
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


(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define sticks (vector->list (dirichlet (vector 1 1 1))))
   (define min_piece (apply min sticks))
   (define p (<= min_piece 1/5))
   
   (list min_piece
         p
         )

   )
)

(displayln "Model 2")
(show-marginals (model2)
                (list  "min_piece"
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


