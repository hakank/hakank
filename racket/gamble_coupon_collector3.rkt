#| 

  Coupon collector problem in Racket Gamble.

  From 
  ## https://www.maa.org/frank-morgans-math-chat-rolling-a-die-until-every-face-has-appeared
  https://sites.williams.edu/Morgan/math-chat-archives/rolling-a-die-until-every-face-has-appeared/
  """
  LD CHALLENGE (Steve Jabloner). How many times do you think you need to roll a
  normal die to be 90% sure that each of the six faces has appeared at least once? Why?

  ANSWER. It takes 23 rolls, as Al Zimmerman discovered in a computer experiment 
  rolling 5000 dice until 90% of them had shown each face. In a less accurate 
  experiment with just 300 dice, Eric Brahinsky wrongly concluded that 22 rolls 
  provide a 90.7% probability. 
  """

  * n=6

var : len
11: 0.08700000000000001
12: 0.08310000000000001
10: 0.07970000000000001
13: 0.07940000000000001
9: 0.07560000000000001
...
52: 0.00020000000000000004
39: 0.00010000000000000002
48: 0.00010000000000000002
49: 0.00010000000000000002
59: 0.00010000000000000002
mean: 14.675600000000001
Credible interval (0.9): 6..23

  The upper value of 90% credible interval is 23. 

  * n=100

var : len
468: 0.005400000000000032
449: 0.005300000000000031
472: 0.00500000000000003
402: 0.0049000000000000285
549: 0.0049000000000000285
...
1018: 0.00010000000000000059
1017: 0.00010000000000000059
1016: 0.00010000000000000059
1022: 0.00010000000000000059
1021: 0.00010000000000000059
mean: 517.7498000000023
Credible interval (0.9): 321..696


  I.e. we have to roll d100 (about) 696 times to be 90% certain that we got all 100 values.

  Related:
   * gamble_coupon_collectors_problem.rkt (it's not a especially good model)
   * gamble_coupon_collectors_problem2.rkt
   * gamble_geometric_cereal_box.rkt
   * (and gamble_coupon_collector_problem.rkt but

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (coupon-collector2 n)
  
  (; enumerate
   rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ; From geometric_cereal_box.rkt
   (define vv (for/list ([i (range n)])
                (add1 (geometric (- 1 (/ i n))))))

   (define v (sum vv))

   (list v)
   )
  )

(show-marginals (coupon-collector2 6)
                (list "len"
                      )
                #:num-samples 10000
                #:truncate-output 5
                #:credible-interval 0.9
                )
