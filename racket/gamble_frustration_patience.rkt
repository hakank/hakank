#| 

  Frustration patience (aka frustration solitaire) in Racket/Gamble

  Grinstead & Snells "Introduction of Probability", page 86
  (https://math.dartmouth.edu/~prob/prob/prob.pdf)
  """
  Recently, a related problem appeared in a column of Marilyn vos Savant.
  Charles Price wrote to ask about his experience playing a certain form of solitaire,
  sometimes called 'frustration solitaire'. In this particular game, a deck of cards
  is shuffled, and then dealt out, one card at a time. As the cards are being dealt,
  the player counts from 1 to 13, and then starts again at 1. (Thus, each number is
  counted four times.) If a number that is being counted coincides with the rank of
  the card that is being turned up, then the player loses the game. Price found that
  he rarely won and wondered how often he should win. Vos Savant remarked that
  the expected number of matches is 4 so it should be difficult to win the game.
  """

  The expected number is 4, but it's quite a range of possible number of matches:

  var : s
  4: 0.2038
  3: 0.1992
  5: 0.1565
  2: 0.143
  6: 0.1085
  1: 0.0701
  7: 0.056
  8: 0.0285
  0: 0.0174
  9: 0.0113
  10: 0.0042
  11: 0.0008
  12: 0.0007
  mean: 3.9833
  Percentiles:
  (0.01 0)
  (0.025 1)
  (0.1 2)
  (0.05 1)
  (0.25 3)
  (0.5 4)
  (0.75 5)
  (0.84 6)
  (0.9 7)
  (0.95 7)
  (0.975 8)
  (0.99 9)
  (0.999 11)

  This is a port of my WebPPL model frustration patience.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define n 52)
(define a (range 1 (add1 n)))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define cards (draw-without-replacement n a))
   (define counts (for/list ([i n])
        (if (= (modulo (list-ref cards i) 13) (modulo (add1 i) 13)) 1 0)))

   (define s (sum counts))

   (list s
         )

   )
)

(show-marginals (model)
              (list  "s"
                     )
                    #:num-samples 1000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    #:show-percentiles? #t
                    )


