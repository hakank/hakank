#| 

  A Machine Probability Puzzle in Racket/Gamble 

  From Cole Frederick: "A Machine Probability Puzzle - Will it ever work?" 
  https://colefp.medium.com/a-machine-probability-puzzle-bbb46322f609
  """
  Here’s a simple puzzle to test your ability to think about probability.

  Imagine you have a machine that works on average every 2 days out of 3. 
  What is the probability that it will work for at least 4 of the next 5 days?
  """

  variable : s
  3: 80/243 (0.3292181069958848)
  4: 80/243 (0.3292181069958848)
  2: 40/243 (0.1646090534979424)
  5: 32/243 (0.13168724279835392)
  1: 10/243 (0.0411522633744856)
  0: 1/243 (0.00411522633744856)
  mean: 10/3 (3.3333333333333335)

  variable : p
  #f: 131/243 (0.5390946502057613)
  #t: 112/243 (0.4609053497942387)
  mean: 112/243 (0.4609053497942387)



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

   (define num-days 5)
   (define working-p 2/3)

   (define days (for/list ([i num-days]) (bernoulli working-p)))
   (define s (sum days))
   (define p (>= s 4))
   
   (list s p)

   )
)

(show-marginals (model)
                (list  "s"
                       "p"
                       ))


