#| 

  False coin in Racket.Gamble 

  From Peter Winkler
  Celebration of Mind (Gathering For Gardner):
  "CoM Apr 2021 - Drawing from Urns - Peter Winkler"
  https://www.youtube.com/watch?v=A0GxvYPTKDk&list=PL5_D5QQxvWKVtX4Vklvhsf5KTYZBLDSwO&index=3

  @3:43
  """
  You have two quarters.
  Both are sitting on a table heads up.
  One of them is an ordinary quarter, but the
  other has a head on the other side as well.

  You get two flips to help you guess which is which.

  Should you flip each coin once or one coin twice.
  """

  According to this model it's better to flip the same coin twice
  than once for each coin. But that's under the condition that
  all the flips shows head. If either flip shows tail then it's
  settled that it's the ordinary coin.

  var : coinA
  1: 0.8
  2: 0.19999999999999996
  mean: 1.2

  var : sideA
  1: 1.0
  mean: 1.0

  var : coinB
  2: 0.8
  1: 0.19999999999999996
  mean: 1.8

  var : sideB
  1: 1.0
  mean: 1.0

  var : coinSameIsFake
  #t: 0.8
  #f: 0.19999999999999996
  mean: 0.8

  var : coinBothIsFake
  #f: 0.8
  #t: 0.19999999999999996
  mean: 0.19999999999999996

  This is a port of my WebPPL model false_coin2.wppl

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

   ;; The two coins
   (define coin1 1) ; fake coin: head,head
   (define coin2 2) ; proper coin: head,tail
   (define coins (vector coin1 coin2))

   ;; Sides of the coins
   (define head 1)
   (define tail 2)

   (define (flip_coin c)
     (if c
         (categorical-vw2 (vector 1.0 0.0) (vector head tail))
         (categorical-vw2 (vector 0.5 0.5) (vector head tail))
         )
     )
   
   ;; The setup: both coins are head (no real flipping)
   (define coinA (uniform-draw coins))
   (define sideA (flip_coin (= coinA coin1)))
   (observe/fail (= sideA head))

   (define coinB (if (= coinA coin1) coin2 coin1))
   (define sideB (flip_coin (= coinB coin1)))
   (observe/fail (= sideB head))

   ;; (observe/fail coinA == coin1 || coinB == coin1)

   ;; Flip same coin
   (define coinSame_1 coinA)
   (define coinSame_1_flip (flip_coin (= coinSame_1 coin1)))
   (define coinSame_2 coinSame_1)
   (define coinSame_2_flip (flip_coin (= coinSame_2 coin1)))
   (observe/fail (= coinSame_1_flip head))
   (observe/fail (= coinSame_2_flip head))
    
   ;; Flip both coins
   (define coinBoth_1 coinA)
   (define coinBoth_1_flip (flip_coin (= coinBoth_1 coin1)))
   (define coinBoth_2 coinB)
   (define coinBoth_2_flip (flip_coin (= coinBoth_2 coin1)))
   (observe/fail (= coinBoth_1_flip head))
   (observe/fail (= coinBoth_2_flip head))
    
    (list coinA
          sideA
          coinB
          sideB
          ;; The probability of fake coin
          (= coinA coin1)
          (= coinB coin1)
    )

   )
)

(show-marginals (model)
              (list "coinA"
                    "sideA"
                    "coinB"
                    "sideB"
                    ;; The probability of fake coin
                    "coinSameIsFake"
                    "coinBothIsFake"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


