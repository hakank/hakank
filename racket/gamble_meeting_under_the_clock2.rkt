#| 

  Meeting under the clock (Julian Simon) in Racket/Gamble 

  """
   Meeting Under the Clock (This problem is posed by Julian Simon(1994))

   Two persons agree to arrive at the two clock sometime between 1 pm and 2 pm 
   and to stay for 20 minutes. What is the probability that they will be there
   at the same time?
   """

   Here we use overlap instead of abs(), 

  variable : c1
  1: 1/60 (0.016666666666666666)
  2: 1/60 (0.016666666666666666)
  3: 1/60 (0.016666666666666666)
  4: 1/60 (0.016666666666666666)
  5: 1/60 (0.016666666666666666)
  ...
  56: 1/60 (0.016666666666666666)
  57: 1/60 (0.016666666666666666)
  58: 1/60 (0.016666666666666666)
  59: 1/60 (0.016666666666666666)
  60: 1/60 (0.016666666666666666)
  mean: 61/2 (30.5)

  variable : c2
  1: 1/60 (0.016666666666666666)
  2: 1/60 (0.016666666666666666)
  3: 1/60 (0.016666666666666666)
  4: 1/60 (0.016666666666666666)
  5: 1/60 (0.016666666666666666)
  ...
  56: 1/60 (0.016666666666666666)
  57: 1/60 (0.016666666666666666)
  58: 1/60 (0.016666666666666666)
  59: 1/60 (0.016666666666666666)
  60: 1/60 (0.016666666666666666)
  mean: 61/2 (30.5)

  variable : prob
  #t: 17/30 (0.5666666666666667)
  #f: 13/30 (0.43333333333333335)
  mean: 17/30 (0.5666666666666667)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (overlap a1 a2 b1 b2) 
  (<= (max a1 b1) (min a2 b2)))
  

(define (model)
  (enumerate

   (define wait_time 20)
   (define c1 (add1 (random-integer 60)))
   (define c2 (add1 (random-integer 60)))

   (define prob (overlap c1 (+ c1 wait_time) c2 (+ c2 wait_time)))
   
   (list c1
         c2
         prob
    )

   )
)

(show-marginals (model)
                (list  "c1"
                       "c2"
                       "prob"
                       )
                )


