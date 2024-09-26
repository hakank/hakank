#| 

  Coin flips in Racket/Gamble 

  https://pedrozudo.github.io/docs/20_thesis/thesis_final_online_compressed.pdf
  page 112
  """
  Example 6.14. We model a coin flip scenario where the prior probability of the coin
  turning up heads is distributed according to a mixture of two beta distributions. DC-
  ProbLog then allows us to learn the posterior distribution by taking into account the
  data in Lines 6 to 8.
  1 0.2::a.
  2 b~beta(1,1):- a.
  3 b~beta(1,2):- \+a.
  4 B::coin_(flip N):- B is b.
  5
  6 evidence(coin_(flip 1), true).
  7 evidence(coin_(flip 2), false).
  8 evidence(coin_(flip 3), true)  
  """

  var : a
  mean: 0.25500000000000017

  var : b
  mean: 0.5327898112918027

  * If we observe a 4th coin flip which is false
  var : a
  mean: 0.19000000000000014

  var : b
  mean: 0.4297240627722347


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
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define a (flip 0.2))
   (define b (if a (beta 1 1) (beta 1 2)))
   
   (define (coin_flip c) (flip b))

   (observe/fail (coin_flip 1))
   (observe/fail (not (coin_flip 2)))
   (observe/fail (coin_flip 3))
   ; (observe/fail (not (coin_flip 4)))

   (list a
         b
    )
   )
)

(show-marginals (model)
                (list  "a"
                       "b"
                       )
                #:num-samples 1000
                #:skip-marginals? #t
                )


