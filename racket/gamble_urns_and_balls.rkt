#| 

  Urns and balls in Racket.Gamble 

  Assignment 1:
  https://edu.swi-prolog.org/mod/assign/view.php?id=242
  """
  http://cplint.eu/p/urns_and_balls.swinb

  Urns and balls

  Suppose you have two urns: urn1 contains 40 blue balls and 20 red balls and urn2 contains 25 
  blue balls and 30 red balls.

  You throw an unbiased coin and, if it turns out head, you draw a ball from the first urn, 
  it it turns out tails you draw a ball from the second urn.

  Write a program modeling this process and a query for answering the question 
  "What is the probability of drawing a blue ball?"
  """

  The exact answer of drawing a blue call is 0.5*40/60 + 0.5*25/55 = 0.56060606060606060606

  var : coin = tail
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  var : coin = head
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  var : draw = red
  #f: 37/66 (0.5606060606060606)
  #t: 29/66 (0.4393939393939394)
  mean: 29/66 (0.4393939393939394)

  var : draw = blue
  #t: 37/66 (0.5606060606060606)
  #f: 29/66 (0.4393939393939394)
  mean: 37/66 (0.5606060606060606)


  This is a port of my WebPPL model urns_and_balls.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define coin (categorical-vw2 (vector 1/2 1/2) (vector "tail" "head")))
   (define colors (vector "blue" "red"))
   
   (define draw (if (eq? coin "head")
                    (categorical-vw2 (vector 40 20) colors)
                    (categorical-vw2 (vector 25 30) colors)))
   
   (list (eq? coin "tail")
         (eq? coin "head")
         (eq? draw "red")
         (eq? draw"blue")
         )

   )
)

(show-marginals (model)
                (list  "coin = tail"
                       "coin = head"
                       "draw = red"
                       "draw = blue"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


