#| 

  Pig food in Racket/Gamble 

  From "Resampling Stats Illustrations" (http://www.statistics101.net/PeterBruce_05-illus.pdf)
  Page 29
  """
  Pig weight gains — reliability of the estimate
  (program “pigfood”)
  (This bootstrap example is from Basic Research Methods in Social
  Science, Julian L. Simon, 1969.)
  An agricultural lab decides to experiment with a new pig ration —
  ration A — on twelve pigs. After 4 weeks, the pigs experience an
  average gain of 508 ounces. The weight gains of the individual
  pigs are as follows: 496, 544, 464, 416, 512, 560, 608, 544, 480, 466,
  512, 496.
  In presenting these results to major agricultural feed distributors,
  the lab wants to report not just the average estimated weight gain
  (as represented by the sample average), but also the possible range
  of sampling error.
  How can we determine the extent to which one sample differs
  from another? (The reliability of our estimated mean weight gain.)
  If we had more time and money, we could try the ration on addi-
  tional groups of 12 pigs, and see how the mean weight gain
  differed from group to group.
  -> 
  interval = 480 to 537 
  """

  variable : samples_mean
  512.1666666666666: 0.01610000000000002
  502.8333333333333: 0.01600000000000002
  505.5: 0.01550000000000002
  506.8333333333333: 0.01520000000000002
  508.1666666666667: 0.015100000000000021
  ...
  515.3333333333334: 0.00010000000000000014
  463.0: 0.00010000000000000014
  463.1666666666667: 0.00010000000000000014
  467.1666666666667: 0.00010000000000000014
  471.1666666666667: 0.00010000000000000014
  mean: 508.1600833333341
  HPD interval (0.84): 488.0..527.0
  Percentiles:
  (0.01 476.0)
  (0.025 481.3333333333333)
  (0.1 489.6666666666667)
  (0.05 485.5)
  (0.25 498.6666666666667)
  (0.5 508.1666666666667)
  (0.75 517.5)
  (0.84 522.6666666666666)
  (0.9 526.6666666666666)
  (0.95 532.0)
  (0.975 536.1666666666666)
  (0.99 541.3333333333334)
  (0.999 550.6666666666666)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define weights '(496 544 464 416 512 560 608 544 480 466 512 496))
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define samples (resample (length weights) weights))
   (define samples_mean (* 1.0 (avg samples)))
   
   (list samples_mean
         )

   )
)

(show-marginals (model)
              (list  "samples_mean"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


