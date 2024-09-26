#| 

  Left some candles in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/216
  """
  You are taking out candies one by one from a jar that has 10 red candies, 
  20 blue candies, and 30 green candies in it. What is the probability 
  that there is at least 1 blue candy and 1 green candy left in the jar when 
  you have taken out all the red candies?

  Assume that the candies of the same color are indistinguishable from one another.
  
  Answer: 7/12 0.583333

  """

var : num-blue
0: 0.3300000000000005
1: 0.23040000000000033
2: 0.1493000000000002
3: 0.11090000000000019
4: 0.07160000000000012
...
11: 0.0015000000000000022
12: 0.0006000000000000009
13: 0.0006000000000000009
15: 0.0002000000000000003
14: 0.00010000000000000015
mean: 1.8355000000000032

var : p-blue
#t: 0.6700000000000004
#f: 0.3300000000000005
mean: 0.6700000000000004

var : num-green
0: 0.2495000000000004
1: 0.19290000000000027
2: 0.13760000000000025
3: 0.11190000000000018
4: 0.08570000000000015
...
17: 0.0007000000000000011
18: 0.0005000000000000008
20: 0.0005000000000000008
19: 0.0004000000000000006
21: 0.00010000000000000015
mean: 2.7643000000000035

var : p-green
#t: 0.7505000000000005
#f: 0.2495000000000004
mean: 0.7505000000000005

var : p
#t: 0.5880000000000001
#f: 0.41200000000000064
mean: 0.5880000000000001


  Cf https://math.stackexchange.com/questions/1805078/candies-withdrawal-probability-for-a-particular-subsequence

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define candles (append (ones-list 10 "red") (ones-list 20 "blue") (ones-list 30 "green")))

(define (model)
  (; enumerate ; out of memory
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define s (shuffle candles))
   (define s (draw-without-replacement (length candles) candles))

   (define (f a)
     (if (not (member "red" a))
         a
         (f (rest a))))

   (define a (f s))

   (define num-blue (count-occurrences-eq "blue" a))
   (define p-blue (> num-blue 0))
   (define num-green (count-occurrences-eq "green" a))
   (define p-green (> num-green 0))
   (define p (and p-blue p-green))
   
   (list num-blue
         p-blue         
         num-green
         p-green
         p
         )

   )
)

(show-marginals (model)
                (list  "num-blue"
                       "p-blue"
                       "num-green"
                       "p-green"
                       "p"
                       
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


