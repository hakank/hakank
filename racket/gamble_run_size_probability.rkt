#| 

  Run size probability in Racket/Gamble 

  What is the probability of a run (number of heads) of x in n tosses,
  with probability p of getting a head?

  * For n=10, p=1/2:
  (0 1 1.0)
  (1 1023/1024 0.9990234375)
  (2 55/64 0.859375)
  (3 65/128 0.5078125)
  (4 251/1024 0.2451171875)
  (5 7/64 0.109375)
  (6 3/64 0.046875)
  (7 5/256 0.01953125)
  (8 1/128 0.0078125)
  (9 3/1024 0.0029296875)
  (10 1/1024 0.0009765625)

  Probability of 20 consecutive success in 100 runs with 90% probability of success
  (probability-of-run-size 100 9/10 10):: 0.7752991958795026


  For the probability of getting n heads after exact k tosses, 
  see gamble_n_heads_in_a_row_after_k_tosses.rkt 

  Also, see gamble_streaks_chess.rkt

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(let ([n 10]
      [p 1/2])
  (displayln "(probability-of-run-size 10 1/2 0..10):")
  (for ([x (range (add1 n))])
    (let ([v (probability-of-run-size n p x)])
      (show2 x v (* 1.0 v))))
  )
(newline)

(displayln "Probability of 20 consecutive success in 100 runs with 90% probability of success")
(show "(probability-of-run-size 100 9/10 10):" (* 1.0 (probability-of-run-size 100 9/10 20)))

#|
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler


   )
)

(show-marginals (model)
              (list  "d"
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

|#
