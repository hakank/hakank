#| 

  The Red and the Black in Racket/Gamble 

  From Martin Gardner (via Julian Simon):
  """
  Problem 1.5: The Red and the Black (from Gardner, 1983, p. 42))
  "Shuffle a packet of four cards - two red, two black - 
   and deal them face down in a row.  Two cards are picked 
   at random, say by placing a penny on each.  What is the 
   probability that those two cards are the same color?"
  """

  var : pick2
  (black red): 1/3 (0.3333333333333333)
  (red black): 1/3 (0.3333333333333333)
  (black black): 1/6 (0.16666666666666666)
  (red red): 1/6 (0.16666666666666666)

  var : p
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)


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

   (define cards (list "red" "red" "black" "black"))
   (define pick2 (draw-without-replacement 2 cards))

   (define p (eq? (first pick2) (second pick2)))
   (list pick2
         p
         )

   )
)

(show-marginals (model)
                (list  "pick2"
                       "p"
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


