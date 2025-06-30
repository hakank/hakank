#| 

  Three people, ten floors in Racket/Gamble 

  From 
  Paolo Molignini: "Three people, ten floors, one puzzle: can you crack it?"
  https://medium.com/puzzle-sphere/three-people-ten-floors-one-puzzle-can-you-crack-it-8ac0ccd88e37
  """
  Three people get into an empty elevator at the first floor of 
  a building that has 10 floors. Each presses the button for 
  their desired floor (unless one of the others has already 
  pressed that button). Assume that they are equally likely to 
  want to go to floors 2 through 10 (independently of each other). 
  What is the probability that the buttons for 3 consecutive floors are pressed?

  ...

  Summarizing, the probability of three consecutive buttons being pressed is then p = 42/729 ≈ 5.76%.

  """

  variable : desired-floors
  (9 6 10): 1/729 (0.0013717421124828531)
  (10 6 8): 1/729 (0.0013717421124828531)
  (10 3 3): 1/729 (0.0013717421124828531)
  (10 3 4): 1/729 (0.0013717421124828531)
  (3 10 9): 1/729 (0.0013717421124828531)
  ...
  (2 9 2): 1/729 (0.0013717421124828531)
  (6 9 2): 1/729 (0.0013717421124828531)
  (2 5 5): 1/729 (0.0013717421124828531)
  (8 6 9): 1/729 (0.0013717421124828531)
  (3 7 2): 1/729 (0.0013717421124828531)

  variable : p
  #f: 229/243 (0.9423868312757202)
  #t: 14/243 (0.05761316872427984)
  mean: 14/243 (0.05761316872427984)


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

   (define num-people 3)
   (define num-floors 10)  ; floors 2,,10

   (define desired-floors (for/list ([p num-people]) (+ 2 (random-integer (- num-floors 1)))))
   (define s (sort desired-floors <))

   ; Probability of consecutive button presses
   (define p (and
              (= 1 (- (third s) (second s)))
              (= 1 (- (second s) (first s)))))
  
   (list desired-floors
         p
         )

   )
)

(show-marginals (model)
                (list  "desired-floors"
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


