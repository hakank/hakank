#| 

  Monty Hall problem in Racket Gamble.

  From Statistics101 (Resampling Stats)
  File MontyHall.txt
  """
  This is the "Three Door Problem" or the "Monty Hall Problem"
  The game show contestant is faced with three doors, behind
  one of which is a prize. He chooses one door, but before
  that door is opened, the host opens another of the three
  doors revealing it to contain nothing. (The host knows which
  door conceals the prize and always opens a door that doesn  t
  have the prize behind it.) The contestant then is given the
  opportunity to switch his choice to the remaining unopened
  door. Should he switch or stay?

  This simulation takes advantage of the fact that if the
  contestant  s guess is wrong, then switching is the winning
  move, and that if the guess is right, then switching loses.

  To understand intuitively why switching is the better option
  consider that at the start, the contestant has 2 out of 3
  chances of guessing wrong. In other words, his first choice
  has a 66.7% probability of being wrong, meaning that there's
  a 66.7% probability that the prize is behind one of the
  other two doors. Therefore, after the empty door is shown,
  the remaining door "inherits" the 66.7% probability. 
  -> 
  stayingWinProbability: 0.3356
  switchingWinProbability: 0.6644

  """

  This model uses three ways of calculating the probabilities:

  * a model based version
 
  var : prizeDoor
  door2: 1/3 (0.3333333333333333)
  door3: 1/3 (0.3333333333333333)
  door1: 1/3 (0.3333333333333333)

  var : guessDoor
  door2: 1/3 (0.3333333333333333)
  door3: 1/3 (0.3333333333333333)
  door1: 1/3 (0.3333333333333333)

  var : stayP
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)

  var : switchP
  #t: 2/3 (0.6666666666666666)
  #f: 1/3 (0.3333333333333333)
  mean: 2/3 (0.6666666666666666)

  * a simulation based version using global variables

  (switching wins: 6 pct: 2/3)
  (staying wins  : 3 pct: 1/3)

  * and a probability based version, simpler model
   Another approach (random-integer 3) != (random-integer 3): #<discrete-dist: [#f 1/3] [#t 2/3]>

  
  Cf gamble_monty_hall.rkt

  This is a port of my WebPPL model monty_hall_problem.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define switchingWinsCount 0)
(define stayingWinsCount 0)
(define doors '("door1" "door2" "door3"))

(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define prizeDoor (uniform-draw doors))
   (define guessDoor (uniform-draw doors))
   (define stayP (if (eq? guessDoor prizeDoor)
                     (and (set! stayingWinsCount (add1 stayingWinsCount)) #t)
                     (and (set! switchingWinsCount (add1 switchingWinsCount)) #f)))
   
   (define switchP (if (not (eq? guessDoor prizeDoor)) #t #f))
    
   (list prizeDoor
         guessDoor
         stayP
         switchP
         )
    )
  )


(show-marginals (model)
                  (list  "prizeDoor"
                         "guessDoor"
                         "stayP"
                         "switchP"
                         )
                  #:num-samples 1000
                  ; #:truncate-output 1
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )

(define totalC (+ switchingWinsCount stayingWinsCount))
(show2 "switching wins:" switchingWinsCount "pct:" (/ switchingWinsCount totalC))
(show2 "staying wins  :" stayingWinsCount "pct:" (/ stayingWinsCount totalC))

;;
;; This is the same as checking the probability that two random integers
;; drawn from 0..2 are not equal: 0.6666666666666666
;;
(displayln (format "\nAnother approach (random-integer 3) != (random-integer 3): ~a"
                   (enumerate (not (= (random-integer 3) (random-integer 3))))
         ))
(newline)
