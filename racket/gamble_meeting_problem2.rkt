#| 

  Meeting problem in Racket.Gamble 

  (I'm not sure about the source. The page I got it from does not exist anymore.)
  """
  Let's say that 4 people agree to meet between 3:00 P.M. and 4:00 P.M.. 
  One man can wait for 10 minutes. Another can wait for 15 minutes. 
  Another can wait for 20 Minutes, and another still can wait for 30 
  minutes. What is the probability that all 4 will meet?
  """

  Note: The meeting must be in the range 3:00pm and 4:00pm.

  This is much slower than gamble_meeting_problem.rkt using enumerate.

  Importance sampler (100000 samples):
  var : prob
  #f: 0.99999
  #t: 1e-5
  mean: 1e-5

  This is a port of my WebPPL model meeting_problem2 .wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(require racket/set) ; for set-intersect

(define (model)
  (; enumerate ; #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define t (add1 60))

   ;; Start times + the wait times
   (define a (random-integer (- t 10))) ;; a will arrive between 0 and 50
   (define a_w (random-integer (add1 10)))

   (define b (random-integer (- t 15)))
   (define b_w (random-integer (add1 15)))
    
   (define c (random-integer (- t 20))) 
   (define c_w (random-integer (add1 20)))
    
   (define d (random-integer (- t 30))) 
   (define d_w (random-integer (add1 30)))
    
   (define prob (=
                 (+ a a_w)
                 (+ b b_w)
                 (+ c c_w)
                 (+ d d_w))
     )
   
   (list prob)

   )
)

(show-marginals (model)
                (list  "prob"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


