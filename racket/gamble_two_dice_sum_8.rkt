#| 

  Rolling two dice which sums to 8 in Racket/Gamble 

  From 
  https://medium.com/math-games/can-you-do-this-not-so-hard-probability-puzzle-40a502dfbc15
  """ 
  Suppose two fair dice are rolled and their sum is 8.
  What's the probability that at least one die lands on 4?
  """

  The probability is 1/5 (0.2)

  variable : p
  #f: 4/5 (0.8)
  #t: 1/5 (0.2)
  mean: 1/5 (0.2)

  variable : d1
  2: 1/5 (0.2)
  3: 1/5 (0.2)
  4: 1/5 (0.2)
  5: 1/5 (0.2)
  6: 1/5 (0.2)
  mean: 4 (4.0)

  variable : d2
  2: 1/5 (0.2)
  3: 1/5 (0.2)
  4: 1/5 (0.2)
  5: 1/5 (0.2)
  6: 1/5 (0.2)
  mean: 4 (4.0)

  variable : s
  8: 1 (1.0)
  mean: 8 (8.0)


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

   (define d1 (add1 (random-integer 6)))
   (define d2 (add1 (random-integer 6)))

   (define s (+ d1 d2))
   
   (observe/fail (= s 8))

   (define p (or (= d1 4) (= d2 4)))

   (list p d1 d2 s)
   
   )
)

(show-marginals (model)
                (list  "p"
                       "d1"
                       "d2"
                       "s"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


