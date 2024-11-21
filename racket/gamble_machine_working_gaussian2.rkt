#| 

  Machine working in Racket/Gamble 

  From https://pedrozudo.github.io/docs/20_thesis/thesis_final_online_compressed.pdf
  page 96
  """
  Example 6.5 (DC-ProbLog Program). This example program models the correct
  working of a machine. The probability distribution of the temperature of the machine
  depends on whether it is a hot day or not.
  1 machine(1).
  2
  3 0.2::hot.
  4 0.99::cooling(1).
  5
  6 temperature ~ normal(27,5):- hot.
  7 temperature ~ normal(20,5):- \+hot.
  8
  9 works(N):- machine(N), cooling(N).
  10 works(N):- machine(N), temperature<25.0.
  11
  12 query(works(1)).
  """

  variable : hot
  mean: 0.2020000000000208

  variable : cooling
  mean: 0.9889999999999475

  variable : temperature
  mean: 21.457307792235024
  HPD interval (0.93): 11.51428427871224..31.761215077712258

  variable : works
  mean: 0.9968999999999466


  This is a port of my WebPPL model machine_working_gaussian2.wppl

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

   (define hot (flip 0.2))
   (define cooling (flip 0.99))

   (define temperature (if hot (normal 27 5) (normal 20 5)))
   (define works (or cooling (< temperature 25)))
   (list hot
         cooling
         temperature
         works
    )
   )
)

(show-marginals (model)
                (list  "hot"
                       "cooling"
                       "temperature"
                       "works"
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


