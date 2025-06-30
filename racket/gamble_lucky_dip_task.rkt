#| 

  Lucky Dip Task in Racket/Gamble 

  From
  Pascal Bercker:
  "Lucky Dip Task — Is This A Fair Game? Three Ways of Modeling with A Bayesian Network"
  https://medium.com/@pbercker/lucky-dip-task-is-this-a-fair-game-three-ways-of-modeling-with-a-bayesian-network-8b393583aa34
  """
  I got this simple puzzle from a dissertation [https://openscholar.uga.edu/record/17924/files/molnar_robert_a_201508_phd.pdf] 
  on teaching conditional probability in a high school context.

  Lucky Dip Task
  Dominic has devised a simple game. Inside a bag he places 3 black and 3 white balls. 
  He then shakes the bag.
  He asks Amy to take two balls from the bag without looking.

  If the two balls are the same color, then Amy wins.
  If they are different colors, then Dominic wins.

  Is the game fair, meaning Dominic and Amy have equal probability if winning?
  If not, then who is most likely to win?
  """

  The game is not fair: 
  Dominic has 3/5 probability of winning, while Amy only has 2/5 probability.

  variable : amy_wins
  #f: 3/5 (0.6)
  #t: 2/5 (0.4)
  mean: 2/5 (0.4)

  variable : dominic-wins
  #t: 3/5 (0.6)
  #f: 2/5 (0.4)
  mean: 3/5 (0.6)

  What if Amy draw _with_ replacement? Then it's a fair game:

  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  variable : dominic-wins
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; (show "amy(1) : hypergeometric2(2 3 6 0)" (hypergeometric2_pdf 2 3 6 0)) ; 1/5
; (show "dominic: hypergeometric2(2 3 6 1)" (hypergeometric2_pdf 2 3 6 1)) ; 3/5
; (show "amy(2) : hypergeometric2(2 3 6 2)" (hypergeometric2_pdf 2 3 6 2)) ; 1/5


(define (model)
  (enumerate

   (define bag '(b b b w w w))

   (define amy-draw (draw-without-replacement 2 bag))

   (define amy-wins (eq? (first amy-draw) (second amy-draw)))
   (define dominic-wins (not amy-wins))

   (list amy-wins dominic-wins)
   
   )
)

(display "\nWithout replacement:\n")
(show-marginals (model)
                (list  "amy_wins"
                       "dominic-wins"
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

#|
  Testing _with_ replacement
|#
(define (model2)
  (enumerate

   (define bag '(b b b w w w))

   (define amy-draw (resample 2 bag))

   (define amy-wins (eq? (first amy-draw) (second amy-draw)))
   (define dominic-wins (not amy-wins))

   (list amy-wins dominic-wins)
   
   )
)

(display "\nWith replacement:\n")
(show-marginals (model2)
                (list  "amy_wins"
                       "dominic-wins"
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


