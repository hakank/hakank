#| 

  Minimum (and maximum) value of 4 dice in Racket/Gamble 

  From Mathematica OrderDistribution
  """
  Four six-sided dice are rolled. Find the expectation of the minimum value:
  -->
  2275/1296
  1.7554

  Find the expectation of the maximum value:
  -->
  6797/1296
  5.2446
  """

  * n=4

  var : min-val
  1: 671/1296 (0.5177469135802469)
  2: 41/144 (0.2847222222222222)
  3: 175/1296 (0.13503086419753085)
  4: 65/1296 (0.05015432098765432)
  5: 5/432 (0.011574074074074073)
  6: 1/1296 (0.0007716049382716049)
  mean: 2275/1296 (1.7554012345679013)

  var : max-val
  6: 671/1296 (0.5177469135802469)
  5: 41/144 (0.2847222222222222)
  4: 175/1296 (0.13503086419753085)
  3: 65/1296 (0.05015432098765432)
  2: 5/432 (0.011574074074074073)
  1: 1/1296 (0.0007716049382716049)
  mean: 6797/1296 (5.244598765432099)


  * For n=1..10, just the mean values

  n: 1
  var : min-val
  mean: 7/2 (3.5)

  var : max-val
  mean: 7/2 (3.5)

  n: 2
  var : min-val
  mean: 91/36 (2.5277777777777777)

  var : max-val
  mean: 161/36 (4.472222222222222)

  n: 3
  var : min-val
  mean: 49/24 (2.0416666666666665)

  var : max-val
  mean: 119/24 (4.958333333333333)

  n: 4
  var : min-val
  mean: 2275/1296 (1.7554012345679013)

  var : max-val
  mean: 6797/1296 (5.244598765432099)

  n: 5
  var : min-val
  mean: 4067/2592 (1.5690586419753085)

  var : max-val
  mean: 14077/2592 (5.4309413580246915)

  n: 6
  var : min-val
  mean: 67171/46656 (1.4397076474622772)

  var : max-val
  mean: 259421/46656 (5.560292352537723)

  n: 7
  var : min-val
  mean: 125587/93312 (1.3458826303155007)

  var : max-val
  mean: 527597/93312 (5.654117369684499)

  n: 8
  var : min-val
  mean: 2142595/1679616 (1.2756457428364578)

  var : max-val
  mean: 9614717/1679616 (5.724354257163542)

  n: 9
  var : min-val
  mean: 456043/373248 (1.2218230238340193)

  var : max-val
  mean: 2156693/373248 (5.778176976165981)

  n: 10
  var : min-val
  mean: 71340451/60466176 (1.1798406269316584)

  var : max-val
  mean: 351922781/60466176 (5.820159373068342)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model [n 4])
  (show "n" n)
  (enumerate

   (define rolls (for/list ([i n]) (add1 (random-integer 6))))

   (define min-val (apply min rolls))
   (define max-val (apply max rolls))   

   (list min-val
         max-val)
      
   )
)

(for ([n (range 1 11)])
  (show-marginals (model n)
                  (list  "min-val"
                         "max-val"
                         )
                  #:skip-marginals? #t
                  )
)

