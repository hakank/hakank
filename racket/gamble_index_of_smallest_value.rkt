#| 

  Probability of index of the smallest value in a list in Racket/Gamble 

  What is the probability of the index of the smallest value in a list.  
  There are two cases, dependending how we handle ties:
  * if a tie: pick the smallest index with the smallest value
  * if a tie: pick any of the indices for the smallest value

  The first approach has a bias towards the first indices:

  var : amin
  0: 979/3125 (0.31328)
  1: 754/3125 (0.24128)
  2: 584/3125 (0.18688)
  3: 454/3125 (0.14528)
  4: 354/3125 (0.11328)
  mean: 188/125 (1.504)

  By randomly picking by indices for the smallest value theres no bias

  var : amin2
  0: 1/5 (0.2)
  1: 1/5 (0.2)
  2: 1/5 (0.2)
  3: 1/5 (0.2)
  4: 1/5 (0.2)
  mean: 2 (2.0)

  This difference is - of course - only relevant when there are duplicate 
  values in the list.

  For n=7 and the biased approach:
  var : amin
  0: 184820/823543 (0.22442058277466992)
  1: 155812/823543 (0.18919716396108036)
  2: 131480/823543 (0.15965165146203658)
  3: 111040/823543 (0.13483206098527944)
  4: 93848/823543 (0.11395640543359606)
  5: 79372/823543 (0.09637869546581053)
  6: 67171/823543 (0.08156343991752707)
  mean: 39330/16807 (2.340096388409591)



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(show-marginals
 (enumerate
  ; importance-sampler ; for n > 7 or so

  (define n 5)
  
  (define q (for/list ([i n]) (add1 (random-integer n))))

  ; Ties are handled by picking the first
  (define amin (argmin2 q))
  (define amax (argmax2 q))  

  ; Ties are handled by picking any of indices for the smallest value
  (define amin2 (argmin2-random-ties q))
  (define amax2 (argmax2-random-ties q))  

  (list amin
        ; amax
        amin2
        ; amax2
        )
  )
 (list "amin"
       ; "amax"
       "amin2"
       ;"amax2"
       )
 #:num-samples 100000
 )

(exit)

