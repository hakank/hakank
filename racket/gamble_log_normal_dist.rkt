#| 

  Log normal distribution in Racket/Gamble 

  From Handbook on probability distributions
  page 50ff

  variable : d
  0.6214080846523617: 0.0009999999999999994
  0.7084574445571292: 0.0009999999999999994
  0.7547918184315006: 0.0009999999999999994
  5.067068139326617: 0.0009999999999999994
  1.6535364861829895: 0.0009999999999999994
  ...
  1.6981069865340366: 0.0009999999999999994
  0.11938463224469242: 0.0009999999999999994
  7.86273307608021: 0.0009999999999999994
  2.8717786823275158: 0.0009999999999999994
  0.3877868442103217: 0.0009999999999999994
  mean: 1.7234699807625316
  Min: 0.03811196152915595 Mean: 1.5839061103980718 Max: 21.103007020677293 Variance: 3.308862740559501 Stddev: 1.819027965854154
  HPD interval (0.84): 0.03811196152915595..2.6992426023016516



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define d (log_normal 0 1))

   (list d
         )
   )
)

(show-marginals (model)
              (list  "d"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


