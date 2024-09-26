#| 

  Craps in Racket/Gamble 

  https://en.wikipedia.org/wiki/Craps
  """
  Craps is a dice game in which players bet on the outcomes of a pair of dice. 
  Players can wager money against each other (playing "street craps") or against 
  a bank ("casino craps").

  ... 
  
  If the come-out roll is 7 or 11, the bet wins.
  If the come-out roll is 2, 3 or 12, the bet loses (known as "crapping out").
  If the roll is any other value, it establishes a point.
  - If, with a point established, that point is rolled again 
    before a 7, the bet wins.
  - If, with a point established, a 7 is rolled before the point is 
    rolled again ("seven out"), the bet loses.
  """

  var : a
  (7): 0.16737000000005792
  (3): 0.055780000000019314
  (11): 0.05520000000001911
  (2): 0.028010000000009697
  (12): 0.027300000000009452
  (8 7): 0.023530000000008145
  (6 7): 0.02279000000000789
  (6 6): 0.01918000000000664
  (8 8): 0.019170000000006637
  (9 7): 0.01898000000000657
  (5 7): 0.01832000000000634
  (10 7): 0.014030000000004858
  (4 7): 0.013880000000004805
  (9 9): 0.012560000000004347
  (5 5): 0.012430000000004302
  (10 10): 0.0072300000000025035
  (4 4): 0.007050000000002441
  (8 6 7): 0.0034100000000011803
  (6 8 7): 0.0031800000000011008
  ...
  (4 9 9 9 7): 1.0000000000003462e-5
  (4 9 6 8 9 7): 1.0000000000003462e-5
  (10 8 4 5 8 7): 1.0000000000003462e-5
  (6 2 10 4 2 4 5 5 10 8 6): 1.0000000000003462e-5
  (8 2 11 9 3 12 5 9 8): 1.0000000000003462e-5

  var : len
  1: 0.3336600000001155
  2: 0.1891500000000655
  3: 0.13557000000004688
  4: 0.09789000000003412
  5: 0.06824000000002309
  6: 0.04877000000001513
  7: 0.03414000000001063
  8: 0.025850000000008085
  9: 0.018830000000005936
  10: 0.013270000000004232
  11: 0.009560000000003096
  12: 0.006810000000002253
  13: 0.004820000000001643
  14: 0.003760000000001312
  15: 0.002790000000000973
  16: 0.001960000000000683
  17: 0.0014400000000005012
  18: 0.0008700000000003021
  20: 0.0006800000000002356
  19: 0.0006500000000002252
  21: 0.0004300000000001486
  22: 0.00016000000000005544
  24: 0.00013000000000004504
  25: 0.00013000000000004504
  23: 0.00012000000000004157
  26: 0.0001100000000000381
  28: 7.000000000002423e-5
  30: 5.000000000001731e-5
  29: 3.0000000000010385e-5
  27: 2.0000000000006924e-5
  32: 1.0000000000003462e-5
  33: 1.0000000000003462e-5
  34: 1.0000000000003462e-5
  37: 1.0000000000003462e-5
  mean: 3.3642500000011237

  var : outcome
  lose: 0.5075200000002622
  win: 0.49248000000021513

  As expected, the bank/casino has a small edge over the customer: it wins with a 
  probability of 5075200000002622.
  In about 1 of 100 000 throws there are very long runs, e.g. more than 30 throws.
  

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate 
   importance-sampler

   (define (game a point)
     (let* ([d1 (add1 (random-integer 6))]
            [d2 (add1 (random-integer 6))]
            [s (+ d1 d2)]
            [new-a (append a (list s))])
       (if (= point 0)
           (cond
             [(or (= s 7) (= s 11)) (list new-a "win") ]
             [(or (= s 2) (= s 3) (= s 12)) (list new-a "lose")]
             [else (game new-a s)])
           (cond
             [(= s 7) (list new-a "lose")]
             [(= s point) (list new-a "win")]
             [else (game new-a point)])
           )))
   (define res (game '() 0))
   (define a (first res))
   (define len (length a))
   (define outcome (second res))

   (list ; a
         len
         outcome
         )
   
   )
)

(show-marginals (model)
                (list  ; "a"
                       "len"
                       "outcome"
                       )
                #:num-samples 100000
                )


