#| 

  Firings in Racket/Gamble 

  From "Resampling Stats Illustrations" (http://www.statistics101.net/PeterBruce_05-illus.pdf)
  Page 54
  """
  Unusual Statistics
  (program “firings”)
  One of the advantages of resampling is its suitability for use with
  non-standard statistics. Here is an illustration of a statistic de-
  signed to meet the needs of a specific situation:
  A company has been accused of firing workers (it has 50) when
  they get close to the level of seniority at which their pension would
  be vested (25 years). The union notes that the levels of seniority of
  7 fired workers in the last 12 months were unusually close to 25
  years.
  Seniority at discharge (years): 23 19 24 23 25 2 5
  Seniority of all workers:
     11 8 24 36 20 19 11 9 10 9 5
     4 2 1 9 21 16 17 11 1 1 23
     19 24 40 28 5 7 1 34 20 16 31
     23 50 4 1 8 8 14 12 32 1 15
     12 25 19 5 24 2
  Note: A “25” indicates the worker’s pension has vested.
  The company counters that operational considerations were the
  only factors in each of the firings and that the proximity of the
  firing dates to pension vesting dates was purely coincidental, the
  result of random chance.
  Can we assess whether this claim is reasonable?
  ...
  Result:
  prob = .11
  The estimated p-value is .11, indicating that a sum as low as the
  observed value of 79 might happen 11% of the time, simply
  drawing workers at random. We conclude the evidence is not
  strong that there was systematic firing of those close to vesting.
  """

  Here we get a little lower probability, but it's still fairly significant: 7.46%

  sum_discharged:: 79
  var : s
  114: 0.02230000000000001
  107: 0.022100000000000012
  110: 0.02170000000000001
  117: 0.02080000000000001
  113: 0.01970000000000001
  ...
  50: 0.0002000000000000001
  33: 0.00010000000000000005
  163: 0.00010000000000000005
  171: 0.00010000000000000005
  43: 0.00010000000000000005
  mean: 109.21749999999996
  Credible interval (0.95): 68..145

  var : p
  #f: 0.9254
  #t: 0.07460000000000003
  mean: 0.07460000000000003


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define seniority_discharged '(23 19 24 23 25 2 5))
(define seniority_all_workers '(11 8 24 36 20 19 11 9 10 9 5 
                                   4 2 1 9 21 16 17 11 1 1 23 
                                   19 24 40 28 5 7 1 34 20 16 31 
                                   23 50 4 1 8 8 14 12 32 1 15 
                                   12 25 19 5 24 2))

(define (sum_seniority data) 
  (sum (map (lambda (v) (if (<= (- 25 v) 0) 25 (- 25 v))) data)))

(define sum_discharged (sum_seniority seniority_discharged))
(show "sum_discharged:" sum_discharged)

(define (model)
  (; enumerate
   rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define discharged_sample (draw-without-replacement (length seniority_discharged) seniority_all_workers))
   (define s (sum_seniority discharged_sample))
   (define p (<= s sum_discharged))
   
   (list s
         p
    )
   )
)

(show-marginals (model)
                (list  "s"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.95
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


