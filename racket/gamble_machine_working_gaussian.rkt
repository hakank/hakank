#| 

  Machine working in Racket/Gamble 

  From https://pedrozudo.github.io/docs/20_thesis/thesis_final_online_compressed.pdf
  page 85f
  """
  Example 6.1. In this example we model the temperature as a continuous random
  variable distributed according to a normal distribution with mean 20 and standard
  deviation 5 (Line 3).
  1 machine(1). machine(2).
  2
  3 temperature ~ normal(20,5).
  4 0.99::cooling(1).
  5 0.95::cooling(2).
  6
  7 works(N):- machine(N), cooling(N).
  8 works(N):- machine(N), temperature<25.0
  evidence(works(2)).
  query(works(1)).
  """

  variable : temperature
  mean: 19.976700547249024
  HPD interval (0.93): 11.083141434497207..29.127755186326638

  variable : cooling 0
  mean: 0.9900999999999474

  variable : cooling 1
  mean: 0.9551999999999512

  variable : works 0
  mean: 0.9987999999999464

  variable : works 1
  mean: 0.9999999999999463

  variable : both_works
  mean: 0.9987999999999464


  This is a port of my WebPPL model machine_working_gaussian.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define temperature (normal 20 5))
   (define cooling (list (flip 99/100) (flip 95/100)))
   (define (works m) 
     (or (list-ref cooling m) (< temperature 25)))
   
   ; Machine 2 works
   (observe/fail (works 1))
   
   (define both_works (and (works 0) (works 1)))
   
   (list temperature
         (list-ref cooling 0)
         (list-ref cooling 1)
         (works 0)
         (works 1)
         both_works
         )
   )
)

(show-marginals (model)
                (list  "temperature"
                       "cooling 0"
                       "cooling 1"
                       "works 0"
                       "works 1"
                       "both_works"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.93)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


