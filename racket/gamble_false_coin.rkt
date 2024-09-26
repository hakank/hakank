#| 

  False coin in Racket.Gamble 

  Inspired by Peter Winkler in
  From Celebration of Mind (Gathering For Gardner):
  "CoM Apr 2021 - Drawing from Urns - Peter Winkler"
  https://www.youtube.com/watch?v=A0GxvYPTKDk&list=PL5_D5QQxvWKVtX4Vklvhsf5KTYZBLDSwO&index=3

  Note: This is not what Peter talked about, but what I - wrongly - remembered. See false_coin2.wppl for this.

  *  We have one fake and one proper coin:
  1. head, head
  2. head, tail

  * We take one coin randomly and flip it: it's a head
  * To identify the fake coin, is it better to flip the same
    coin or flip the other coin?

  According to this model, it's better to throw the same coin again, i.e.
  coinB_switchIsFake: 1/3
  vs
  coinB_sameIsFake: 2/3


  var : coinA
  1: 0.6666666666666666
  2: 0.3333333333333334
  mean: 1.3333333333333335

  var : sideA
  1: 1.0
  mean: 1.0

  var : coinB_switch
  2: 0.6666666666666666
  1: 0.3333333333333334
  mean: 1.6666666666666667

  var : sideB_switch
  1: 0.6666666666666667
  2: 0.3333333333333333
  mean: 1.3333333333333335

  var : coinB_same
  1: 0.6666666666666666
  2: 0.3333333333333334
  mean: 1.3333333333333335

  var : sideB_same
  1: 0.8333333333333334
  2: 0.1666666666666667
  mean: 1.1666666666666667

  var : coinB_switchIsFake
  #f: 0.6666666666666666
  #t: 0.3333333333333334
  mean: 0.3333333333333334

  var : coinB_sameIsFake
  #t: 0.6666666666666666
  #f: 0.3333333333333334
  mean: 0.6666666666666666


  Note: The problem that Peter Winkler actually talked about was a little different:
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

  See gamble_false_coin2.rkt for this version.

  This is a port of my WebPPL model false.coin.wppl

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
         (categorical-vw2 (vector 0.5 0.5) (vector head tail))))
   
   ;; 1. Pick a coin randomly and flip it. It's head.
   (define coinA (uniform-draw coins))
   (define sideA (flip_coin (= coinA coin1)))
   
   ;; We observe that the first flip was a head.
   (observe/fail (= sideA head))
   
   ;; 2a: Take the other coin and flip it.
   (define coinB_switch (if (= coinA coin1) coin2 coin1))
   (define sideB_switch (flip_coin (= coinB_switch coin1)))
   
   ;; 2b: Take the same coin and flip it.
   (define coinB_same coinA)
   (define sideB_same (flip_coin (= coinB_same coin1)))
   
   (list coinA
         sideA
         coinB_switch
         sideB_switch
         coinB_same
         sideB_same
         ;; The probability of fake coin
         (= coinB_switch coin1)
         (= coinB_same coin1)
    )

   )
)

(show-marginals (model)
              (list "coinA"
                    "sideA"
                    "coinB_switch"
                    "sideB_switch"
                    "coinB_same"
                    "sideB_same"
                    ;; The probability of fake coin
                    "coinB_switchIsFake"
                    "coinB_sameIsFake"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


