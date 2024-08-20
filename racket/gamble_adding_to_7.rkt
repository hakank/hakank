#| 

  Adding to 7 in Racket Gamble.

  From   https://www.youtube.com/watch?v=6Lqt07enBGs&list=PLPhJUsEbNh6T7TZ-LPRZgepphZQIB0A02&index=6&t=1627s
  "Probabilistic Programming and Bayesian Nonparametrics -- Frank Wood (Part 1)"
  @1:21:01
  """  
   [assume a (- (poisson 100) 100)]
   [assume b (- (poisson 100) 100)]
   [observe (normal (+ a b) 0.00001) 7]
   [predict (list a b)]
  """

  * importance-sampler

var : a
6: 0.05719999999999992
1: 0.05519999999999992
3: 0.054999999999999924
4: 0.05399999999999993
...
29: 0.00020000000000000088
-19: 0.00020000000000000088
-20: 0.00020000000000000088
28: 0.00010000000000000044
mean: 3.5392
Histogram:
-26: 505
-24: 532
-22: 514
-21: 535
-20: 591
-19: 533
-18: 537
-17: 508
-16: 490
-15: 431
-14: 400
-13: 289
-12: 242
-11: 234
-10: 175
 -9: 134
 -8: 132
 -7: 102
 -6: 71 
 -5: 56 
 -4: 39 
 -3: 29 
 -2: 25 
 -1: 12 
  0: 10 
  1: 4  
  2: 3  
  3: 4  
  4: 3  
  5: 2  
  6: 2  
  7: 5  
  8: 7  
  9: 3  
 10: 4  
 11: 4  
 12: 3  
 13: 3  
 14: 2  
 15: 436
 16: 433
 17: 375
 18: 338
 19: 283
 20: 209
 21: 189
 22: 165
 23: 121
 24: 82 
 25: 81 
 26: 60 
 27: 40 
 28: 27 
 29: 24 
 30: 17 

var : b
1: 0.05719999999999992
6: 0.05519999999999992
4: 0.054999999999999924
3: 0.05399999999999993
...
26: 0.00020000000000000088
27: 0.00020000000000000088
-22: 0.00020000000000000088
-21: 0.00010000000000000044
mean: 3.4608
Histogram:
-23: 508
-22: 537
-21: 533
-20: 591
-19: 535
-18: 514
-17: 532
-16: 505
-15: 436
-14: 433
-13: 375
-12: 338
-11: 283
-10: 209
 -9: 189
 -8: 165
 -7: 121
 -6: 82 
 -5: 81 
 -4: 60 
 -3: 40 
 -2: 27 
 -1: 24 
  0: 17 
  1: 5  
  2: 7  
  3: 3  
  4: 4  
  5: 4  
  6: 3  
  7: 3  
  8: 10 
  9: 4  
 10: 2  
 11: 3  
 12: 4  
 13: 3  
 14: 2  
 15: 2  
 16: 490
 17: 431
 18: 400
 19: 289
 20: 242
 21: 234
 22: 175
 23: 134
 24: 132
 25: 102
 26: 71 
 27: 56 
 28: 39 
 29: 29 
 31: 25 
 33: 12 

var : a+b
7: 1.0000000000000002
mean: 7.000000000000002
Histogram:
7: 10001




  This is a port of my WebPPL adding_to_7.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 100)
   
   (define a (- (poisson n) n))
   (define b (- (poisson n) n))

   (observe-sample (normal-dist (+ a b) 0.0001) 7)
  
   (list a
         b
         (+ a b)
         )

   )
  )

(show-marginals (model)
                (list "a"
                      "b"
                      "a+b"
                        )
                  #:num-samples 1000
                  #:truncate-output 4
                  ; #:skip-marginals? #t
                  #:show-histogram? #t
                  )
