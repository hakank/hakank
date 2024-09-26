#| 

  Rolling dice problem in Racket.Gamble 

  https://dtai.cs.kuleuven.be/problog/tutorial/basic/03_dice.html
  """
  The following example illustrates the use of a logic program with recursion and lists. 
  We start by rolling the first die. Every roll determines the next die to roll, but we stop 
  if we have used that die before. We query for the possible sequences of rolled dice. 
  We use three-sided dice instead of the regular six-sided ones simply to restrict the number 
  of possible outcomes (and thus inference time). 
  """

  Let's use 1..4

  * Without any observation

  var : a
  (3 3): 1/16 (0.0625)
  (2 2): 1/16 (0.0625)
  (4 4): 1/16 (0.0625)
  (1 1): 1/16 (0.0625)
  (3 2 3): 1/64 (0.015625)
  ...
  (2 2 3 4 1): 1/1024 (0.0009765625)
  (2 3 1 4 2): 1/1024 (0.0009765625)
  (3 3 2 1 4): 1/1024 (0.0009765625)
  (4 2 3 4 1): 1/1024 (0.0009765625)
  (1 2 1 3 4): 1/1024 (0.0009765625)
 
  var : len
  3: 3/8 (0.375)
  4: 9/32 (0.28125)
  2: 1/4 (0.25)
  5: 3/32 (0.09375)
  mean: 103/32 (3.21875)

  * Observation: the first roll is 1.

  var : a
  (1 1): 1/4 (0.25)
  (1 2 1): 1/16 (0.0625)
  (1 1 3): 1/16 (0.0625)
  (1 1 4): 1/16 (0.0625)
  (1 4 1): 1/16 (0.0625)
  ...
  (1 4 2 3 1): 1/256 (0.00390625)
  (1 3 2 4 1): 1/256 (0.00390625)
  (1 2 3 4 1): 1/256 (0.00390625)
  (1 3 1 4 2): 1/256 (0.00390625)
  (1 2 1 3 4): 1/256 (0.00390625)

  var : len
  3: 3/8 (0.375)
  4: 9/32 (0.28125)
  2: 1/4 (0.25)
  5: 3/32 (0.09375)
  mean: 103/32 (3.21875)


  * For N=6 (no observation)

  var : a
  (4 4): 1/36 (0.027777777777777776)
  (5 5): 1/36 (0.027777777777777776)
  (2 2): 1/36 (0.027777777777777776)
  (3 3): 1/36 (0.027777777777777776)
  (6 6): 1/36 (0.027777777777777776)
  ...
  (2 1 2 4 5 6 3): 1/279936 (3.5722450845907635e-6)
  (2 2 4 5 6 3 1): 1/279936 (3.5722450845907635e-6)
  (1 4 6 5 3 2 1): 1/279936 (3.5722450845907635e-6)
  (2 2 3 1 5 6 4): 1/279936 (3.5722450845907635e-6)
  (3 6 2 1 4 3 5): 1/279936 (3.5722450845907635e-6)

  var : len
  3: 5/18 (0.2777777777777778)
  4: 5/18 (0.2777777777777778)
  5: 5/27 (0.18518518518518517)
  2: 1/6 (0.16666666666666666)
  6: 25/324 (0.07716049382716049)
  7: 5/324 (0.015432098765432098)
  mean: 1223/324 (3.7746913580246915)


  * N=9 (enumerate 52.9s)
var : len
4: 56/243 (0.23045267489711935)
5: 448/2187 (0.20484682213077274)
3: 16/81 (0.19753086419753085)
6: 2800/19683 (0.14225473759081442)
2: 1/9 (0.1111111111111111)
7: 4480/59049 (0.07586919338176769)
8: 15680/531441 (0.029504686315131878)
9: 35840/4782969 (0.00749325366733508)
10: 4480/4782969 (0.000936656708416885)
mean: 21323986/4782969 (4.458315744885656)

  importance-sampler (2.2s)
  var : len
  4: 0.23015000000000002
  5: 0.20394000000000004
  3: 0.19710000000000003
  6: 0.14327000000000004
  2: 0.11126000000000001
  7: 0.07560000000000001
  8: 0.030060000000000003
  9: 0.007710000000000001
  10: 0.0009100000000000001
  mean: 4.4619100000000005


  * N=10 (too slow for enumerate, using importance-sampler)

  var : len
  4: 0.21863
  5: 0.20146
  3: 0.18117
  6: 0.15084
  2: 0.09809
  7: 0.08936
  8: 0.04213
  9: 0.01466
  10: 0.00339
  11: 0.00027
  mean: 4.657920000000001

  This is a port of my WebPPL rolling_dice5.wppl

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

   ; (define N 3)
   (define N 4)
   ; (define N 6)
   ; (define N 9)
   ; (define N 10) ; importance-sampler
    
   (define (roll arr)
     (if (not (= (length arr) (length (remove-duplicates arr)))) 
         arr
         (roll (cons (add1 (random-integer N)) arr))))

   (define a (roll '()))

   ;; In the example, the first roll is 1.
   ; (observe/fail (= (list-ref a 0) 1))
    
   (list ; a
         (length a)
         )
    
   )
)

(show-marginals (model)
                (list  ; "a"
                       "len"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
