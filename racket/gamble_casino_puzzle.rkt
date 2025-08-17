#| 

  Casino puzzle in Racket/Gamble 

  From 
  Muhammad Zain Sarwar
  "The Casino Puzzle That Breaks Your Expectations!"
  https://medium.com/puzzle-sphere/youll-never-guess-how-long-it-takes-to-lose-all-your-money-3a7d57e7ac98
  """
  You find yourself at a casino in Las Vegas, and the dealer presents you with a game. 
  The rules are simple:

  You roll six dice at the same time and each dice is fair meaning every face 
  (1, 2, 3, 4, 5, or 6) has an equal chance of appearing.

  You count how many different numbers appear on the dice.

  If exactly four distinct numbers show up, then you win $1.

  Otherwise, you lose $1.

  You start with $100 and play until you run out of money.

  Each round takes one minute to play.

  Puzzle Statement

  How long do you think it will take for you to lose all your money?
  """

  Here's the exact probability of winning
  variable : d
  4: 325/648 (0.5015432098765432)
  3: 25/108 (0.23148148148148148)
  5: 25/108 (0.23148148148148148)
  2: 155/7776 (0.01993312757201646)
  6: 5/324 (0.015432098765432098)
  1: 1/7776 (0.0001286008230452675)
  mean: 31031/7776 (3.9906121399176953)

  variable : p
  #t: 325/648 (0.5015432098765432)
  #f: 323/648 (0.4984567901234568)
  mean: 325/648 (0.5015432098765432)

  Since p > 0.5 the probability of loosing in the long run, is (on average) 0.


  But let's simulate this (model2), by setting the two stop cases to 
  0 or 200 so we can see the average number of runs needed.

  model2
  variable : v
  200: 0.6600000000000005
  0: 0.34000000000000025
  mean: 132.00000000000009

  variable : c
  3410: 0.0039999999999999975
  4222: 0.0029999999999999983
  4768: 0.0029999999999999983
  10290: 0.0019999999999999987
  4144: 0.0019999999999999987
  ...
  2044: 0.0009999999999999994
  9182: 0.0009999999999999994
  6122: 0.0009999999999999994
  10200: 0.0009999999999999994
  11220: 0.0009999999999999994
  mean: 9497.791999999987


  The probability of ending when reaching 200 in this setup is thus 
  about 0.66 and loosing is about 0.34.

  The mean number of runs is about 9500 runs.
  (The run takes almost 3 minutes.)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 6) 

   (define game (for/list ([i n]) (add1 (random-integer 6))))

   (define d (length (remove-duplicates game)))

   (define p (= d 4))
   
   (list d p)

   )
)

#|
(displayln "model1")
(show-marginals (model1)
                (list  "d"
                       "p"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )
|#



(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 6) 

   (define (throw) (length (remove-duplicates (for/list ([i n]) (add1 (random-integer 6))))))
   
   (define (f v c)
     (if (or (<= v 0) (>= v 200))
         (list v c)
         (if (= (throw) 4)
             (f (+ v 1) (+ 1 c))
             (f (- v 1) (+ 1 c))
             )
     ))
   
   (define a (f 100 0))
   (define v (first a))
   (define c (second a))
   
   (list v c)

   )
)

(displayln "model2")
(show-marginals (model2)
                (list  "v"
                       "c"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )




