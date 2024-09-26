#| 

  Righest dice roller in Racket.Gamble 

  From https://www.reddit.com/r/Probability/comments/1dxtfe5/probability_of_being_the_highest_roller/
  """
  Probability of being the highest roller

  What are the probability percentages for each person rolling the highest number 
  when all three each of them rolls their specific die once. In case of a tie, 
  the person with the largest number of sided die gets the tiebreaker.

  Person 1 rolls a 12 sided die Person 2 rolls an 8 sided die Person 3 rolls a 4 sided die.
  """

  var : winner
  d12: 131/192 (0.6822916666666666)
  d8: 9/32 (0.28125)
  d4: 7/192 (0.036458333333333336)


  * Now, let's say d8 is the winner:

  var : winner
  d8: 1 (1.0)

  var : d4
  1: 7/27 (0.25925925925925924)
  2: 7/27 (0.25925925925925924)
  3: 1/4 (0.25)
  4: 25/108 (0.23148148148148148)
  mean: 265/108 (2.4537037037037037)

  var : d8
  8: 7/27 (0.25925925925925924)
  7: 2/9 (0.2222222222222222)
  6: 5/27 (0.18518518518518517)
  5: 4/27 (0.14814814814814814)
  4: 1/9 (0.1111111111111111)
  3: 1/18 (0.05555555555555555)
  2: 1/54 (0.018518518518518517)
  mean: 331/54 (6.12962962962963)

  var : d12
  1: 25/108 (0.23148148148148148)
  2: 23/108 (0.21296296296296297)
  3: 5/27 (0.18518518518518517)
  4: 4/27 (0.14814814814814814)
  5: 1/9 (0.1111111111111111)
  6: 2/27 (0.07407407407407407)
  7: 1/27 (0.037037037037037035)
  mean: 331/108 (3.064814814814815)

  * And is d4 is the winner

  var : winner
  d4: 1 (1.0)

  var : d4
  4: 9/14 (0.6428571428571429)
  3: 2/7 (0.2857142857142857)
  2: 1/14 (0.07142857142857142)
  mean: 25/7 (3.5714285714285716)

  var : d8
  1: 3/7 (0.42857142857142855)
  2: 5/14 (0.35714285714285715)
  3: 3/14 (0.21428571428571427)
  mean: 25/14 (1.7857142857142858)

  var : d12
  1: 3/7 (0.42857142857142855)
  2: 5/14 (0.35714285714285715)
  3: 3/14 (0.21428571428571427)
  mean: 25/14 (1.7857142857142858)


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

   (define d4 (add1 (random-integer 4)))
   (define d8 (add1 (random-integer 8)))
   (define d12 (add1 (random-integer 12)))
   
   (define winner
     (cond 
       [(and (> d4 d8) (> d4 d12)) "d4"]
       [(and (>= d8 d4) (> d8 d12)) "d8"]
       [(and (>= d12 d4) (>= d12 d8)) "d12"]
       [else "someone else"]))

   ; (observe/fail (eq? winner "d8"))
   ; (observe/fail (eq? winner "d4"))   

   (list winner
         d4
         d8
         d12
         )
           
   
   
   )
)

(show-marginals (model)
                (list  "winner"
                       "d4"
                       "d8"
                       "d12"
                     )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


