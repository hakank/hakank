#| 

  Two coins in Racket/Gamble 

  This is a port of the R2 model TwoCoins.cs
  
  Output from the R2 model:
  '''
  (0) Mean: 0.339              firstCoin
  (0) Variance: 0.224303
  (1) Mean: 0.309              secondCoin
  (1) Variance: 0.213733
  Number of accepted samples = 847
  '''

  variable : first_coin
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)

  variable : second_coin
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)

  variable : bothHeads
  #f: 1 (1.0)
  mean: 0 (0.0)

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
   (define firstCoin  (flip 1/2))
   (define secondCoin (flip 1/2))
   (define bothHeads (and firstCoin secondCoin))
   (observe/fail (not bothHeads))
  
   (list firstCoin
         secondCoin
         bothHeads
         )
    
   )
)

(show-marginals (model)
                (list  "first_coin"
                       "second_coin"
                       "bothHeads"
                     ))


