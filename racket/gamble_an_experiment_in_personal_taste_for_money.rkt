#| 

  An experiment in Personal Taste for Money (Mosteller) in Racket/Gamble 

  From Mosteller "Fifty Challenging problems in Probability"
  """
  10. An Experiment in Personal Taste for Money
  (a) An urn contains 10 black balls and 10 white balls, identical except for
  color. You choose "black" or "white." One ball is drawn at random. and if its
  color matches your choice. you get $10. otherwise nothing. Write down the
  maximum amount you are willing to pay to play the game. The game will be
  played just once.
  (b) A friend of yours has available many black and many white balls. and he
  puts black and white balls into the urn to suit himself. You choose "black" or
  "white." A ball is drawn randomly from this urn. Write down the maximum
  amount you are willing to pay to play this game. The game will be played
  just once.
  """

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define selected "white")
   
   (define urn (append (rep 10 "black") (rep 10 "white")))

   (define draw (uniform-draw urn))

   (define outcome (if (equal? selected draw) 10 0))
  
   (list draw outcome)

   )
)

(show-marginals (model1)
                (list  "draw"
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

#|

  b) second part.

  If the friend selects randomly between equally number of black and white balls,
  it's the same outcome as in a).

  However, if there are a different number of available of white and black
  balls, then it's a little different outcome.

  For example if the friend has 10 black balls and 20 white balls and
  random select at least one from each and then randomly some more, 
  the outcome is:

  Model 2
  variable : draw
  white: 58129851106297/93163582512000 (0.623954656303707)
  black: 35033731405703/93163582512000 (0.3760453436962931)

  variable : outcome
  10: 58129851106297/93163582512000 (0.623954656303707)
  0: 35033731405703/93163582512000 (0.3760453436962931)
  mean: 58129851106297/9316358251200 (6.239546563037069)


|#
(displayln "\nModel 2")
(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define num-black (add1 (random-integer 10)))
   (define num-white (add1 (random-integer 20)))  
   
   (define selected "white")
   
   (define urn (append (rep num-black "black") (rep num-white "white")))

   (define draw (uniform-draw urn))

   (define outcome (if (equal? selected draw) 10 0))
  
   (list draw outcome)

   )
)

(show-marginals (model2)
                (list  "draw"
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


