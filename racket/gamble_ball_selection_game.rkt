#| 

  Ball selection game in Racket/Gamble 

  Thanushan Sivapatham:
  "Probability Analysis: Resolving Conditional and Total Probabilities in Tim and David’s Ball Selection Game"
  https://medium.com/puzzle-sphere/probability-analysis-tim-and-davids-ball-selection-game-solving-conditional-and-total-cc6ae1c5cab1
  """
  Friends, Tim and David, are playing a game at a party. 
  Box C contains 3 red balls and 4 black balls, while 
  Box D contains 4 red balls and 3 back balls.
  The ball in boxes A [C!] amd B [D!] are identical in all
  respects, except for their color.
  Tim takes a ball at random from Box C and puts it into
  Box D. Then, David takes a ball at random from Box D.

  Q1) What is the probability that the ball taken out of Box D
      by David is black?
  Q2) What is the probability that the ball taken out of Box C 
      by Tim is black, given that the ball taken out of Box D
      by David is red?
  
  """
  Q1) What is the probability that the ball taken out of Box D
      by David is black?

     variable : draw1
     black: 4/7 (0.5714285714285714)
     red: 3/7 (0.42857142857142855)

     variable : draw2
     red: 31/56 (0.5535714285714286)
     black: 25/56 (0.44642857142857145)

     Answer: 25/56

  Q2) What is the probability that the ball taken out of Box C 
      by Tim is black, given that the ball taken out of Box D
      by David is red?

      variable : draw1
      black: 16/31 (0.5161290322580645)
      red: 15/31 (0.4838709677419355)

      variable : draw2
      red: 1 (1.0)

      Answer: 16/31


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

   (define boxC '(red red red black black black black))
   (define boxD '(red red red red black black black))

   (define draw1 (uniform-draw boxC))
   (define boxD2 (append boxD (list draw1)))

   (define draw2 (uniform-draw boxD2))

   ; For Q2
   ; (observe/fail draw2 'red)
   
   (list draw1 draw2)

   )
)

(show-marginals (model)
                (list  "draw1"
                       "draw2"
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


