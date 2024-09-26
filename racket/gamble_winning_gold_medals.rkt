#| 

  Winning medals in Racket.Gamble 

  From https://www.reddit.com/r/Probability/comments/1e5lhgf/probability_problem_w_no_solution/
  """
  Probability Problem w/ no solution?

  The problem:
  "olympic athletes from the US and China are among those most likely to win a gold metal. 
  For a random athlete from either of these two countries, you have the following information: 
  the probability of winning a gold medal if they practiced at least five hours per day = 0.6 
  the probability of winning a gold meal if they practiced less than five hours per day = 0.3 
  if an american athlete wins a gold medal, what is the probability that they practiced at 
  least five hours per day?"
  Select only one:
  1/3, 2/5, 1/2, 3/5

  I suspect that the problem as stated can't have exact answer but instead a possible range. 
  But by doing the calculations and using Bayes, I think that the problem statement is totally wrong...
  """

  For this model, it depends on how many possible hours an athlete can train per day.

  * Here for 0..10 hours/day

  var : practice-time-per-day
  5: 0.13333333333333333
  6: 0.13333333333333333
  7: 0.13333333333333333
  8: 0.13333333333333333
  9: 0.13333333333333333
  0: 0.06666666666666667
  1: 0.06666666666666667
  2: 0.06666666666666667
  3: 0.06666666666666667
  4: 0.06666666666666667
  mean: 5.333333333333334

  var : gold-medal
  #t: 0.9999999999999999
  mean: 0.9999999999999999

  var : p
  #t: 0.6666666666666666
  #f: 0.3333333333333333
  mean: 0.6666666666666666

  * Here for 0..20 hours/day
  var : practice-time-per-day
  5: 0.05714285714285714
  6: 0.05714285714285714
  7: 0.05714285714285714
  8: 0.05714285714285714
  9: 0.05714285714285714
  10: 0.05714285714285714
  11: 0.05714285714285714
  12: 0.05714285714285714
  13: 0.05714285714285714
  14: 0.05714285714285714
  15: 0.05714285714285714
  16: 0.05714285714285714
  17: 0.05714285714285714
  18: 0.05714285714285714
  19: 0.05714285714285714
  0: 0.028571428571428567
  1: 0.028571428571428567
  2: 0.028571428571428567
  3: 0.028571428571428567
  4: 0.028571428571428567
  mean: 10.57142857142857

  var : gold-medal
  #t: 1.0000000000000002
  mean: 1.0000000000000002

  var : p
  #t: 0.8571428571428573
  #f: 0.14285714285714285
  mean: 0.8571428571428573

  * And for max 12 hours per day

  var : practice-time-per-day
  5: 0.10526315789473684
  6: 0.10526315789473684
  7: 0.10526315789473684
  8: 0.10526315789473684
  9: 0.10526315789473684
  10: 0.10526315789473684
  11: 0.10526315789473684
  0: 0.05263157894736842
  1: 0.05263157894736842
  2: 0.05263157894736842
  3: 0.05263157894736842
  4: 0.05263157894736842
  mean: 6.421052631578947

  var : gold-medal
  #t: 1.0
  mean: 1.0

  var : p
  #t: 0.7368421052631579
  #f: 0.2631578947368421
  mean: 0.7368421052631579

  The above assumes that the athlete train exactly integer hours. 

  Here we change to a uniform 0 max-hours-per-day.

  * For max-hours-per-day = 10 (uniform)

  var : practice-time-per-day
  mean: 5.828350571886853

  var : gold-medal
  #t: 1.0000000000000007
  mean: 1.0000000000000007

  var : p
  #t: 0.6650000000000005
  #f: 0.33500000000000024
  mean: 0.6650000000000005

  * For 0..12.0 hours/day

  var : practice-time-per-day
  mean: 7.069723361472238

  var : gold-medal
  #t: 1.0000000000000007
  mean: 1.0000000000000007

  var : p
  #t: 0.7540000000000006
  #f: 0.2460000000000002
  mean: 0.7540000000000006

  * And for 0..20.0 hours/day
  var : practice-time-per-day
  mean: 10.979247789751627

  var : gold-medal
  #t: 1.0000000000000007
  mean: 1.0000000000000007

  var : p
  #t: 0.8490000000000006
  #f: 0.1510000000000001
  mean: 0.8490000000000006

  Unsurprisingly, these values are about the same as for the integer distribution.

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

   ; We don't care about the country since it seems to be irrelevant.

   (define max-hours-per-day 10)
   
   (define practice-time-per-day (random-integer max-hours-per-day)) ;
   ; (define practice-time-per-day (uniform 0 max-hours-per-day)) ;

   (define gold-medal
     (cond
       [(< practice-time-per-day 5) (flip 0.3)]  ; less than 5 hours per day
       [(>= practice-time-per-day 5) (flip 0.6)]) ; at least 5 hours per day
     )

   ; "if an american athlete wins a gold medal, what is the probability
   ; that they practiced at least five hours per day?""
   (define p (>= practice-time-per-day 5))

   ; We observe that the athlete won an gold medal
   (observe/fail (eq? gold-medal #t))

   (list practice-time-per-day
         gold-medal
         p
         )

   )
)

(show-marginals (model)
                (list  "practice-time-per-day"
                       "gold-medal"
                       "p"
                     )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


