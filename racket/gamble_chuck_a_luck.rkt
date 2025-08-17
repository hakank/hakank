#| 

  Chuck-a-luck (Mosteller) in Racket/Gamble 

  From Mosteller "Fifty Challenging problems in Probability"
  """
  Chuck-a-Luck is a gambling game often played at carnivals and gambling
  houses. A player may bet on anyone of the numbers I, 2, 3, 4, 5, 6. Three
  dice are rolled. If the player's number appears on one, two, or three of the
  dice, he receives respectively one, two, or three times his original stake plus
  his own money back; otherwise he loses his stake. What is the player's
  expected loss per unit stake? (Actually the player may distribute stakes on
  several numbers, but each such stake can be regarded as a separate bet.)

  ...

  Thus you lose about 8% [17/216 ~ ~0.079] per play.
  """

  This model gives the same exact loss as -17/216 -0.0787037037037037.

  variable : num-hits
  0: 125/216 (0.5787037037037037)
  1: 25/72 (0.3472222222222222)
  2: 5/72 (0.06944444444444445)
  3: 1/216 (0.004629629629629629)
  mean: 1/2 (0.5)

  variable : outcome
  -1: 125/216 (0.5787037037037037)
  1: 25/72 (0.3472222222222222)
  2: 5/72 (0.06944444444444445)
  3: 1/216 (0.004629629629629629)
  mean: -17/216 (-0.0787037037037037)


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

   (define n 3) ; num dice to roll

   (define user-num (add1 (random-integer 6)))
   (define roll (for/list ([i 3]) (add1 (random-integer 6))))

   (define num-hits (sum (map (lambda (v) (b2i (= user-num v))) roll)))

   ; Outcome. We assume the user bets 1 unit
   (define outcome (if (> num-hits 0) num-hits -1))
   
   (list num-hits outcome)

   )
)

(show-marginals (model)
                (list  "num-hits"
                       "outcome"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


