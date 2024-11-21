#| 

  Placebo and drugs in Racket/Gamble 

  From Dennis Shasha and Manda Wilson: Statistics is Easy!
  https://cs.nyu.edu/~shasha/papers/StatisticsIsEasyExcerpt.html
  """
  Imagine we have given some people a placebo and others a drug. The measured 
  improvement (the more positive the better) is:
  Placebo: 54 51 58 44 55 52 42 47 58 46
  Drug:    54 73 53 70 73 68 52 65 65
  As you can see, the drug seems more effective on the average (the average measured 
  improvement is nearly 63.7 (63 2/3 to be precise) for the drug and 50.7 for the 
  placebo). But, is this difference in the average real?
  ...
  We repeat this shuffle-then-measure procedure 10,000 times and ask what fraction of time 
  we get a difference between drug and placebo greater than or equal to the measured difference 
  of 63.7 - 50.7 = 13. The answer in this case is under 0.001. That is less than 0.1%. Therefore, 
  we conclude that the difference between the averages of the samples is real. 
  This is what statisticians call significant. 
  """

  (placebo_mean: 50.7 drug_mean: 63.666666666666664 diff_mean: 12.966666666666661)
  all:: (54 51 58 44 55 52 42 47 58 46 54 73 53 70 73 68 52 65 65)
  labels:: (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)

  Here are two models_

  * using shuffle (and 100000 samples)

  variable : sample_placebo
  mean: 56.83669800000004
  HPD interval (0.9): 53.1..60.0
  HPD interval (0.99): 51.6..61.9
  HPD interval (0.999): 50.6..63.3
  HPD interval (0.9999): 49.9..63.8

  variable : sample_drug
  mean: 56.848113333333366
  HPD interval (0.9): 52.77777777777778..60.44444444444444
  HPD interval (0.99): 51.0..62.44444444444444
  HPD interval (0.999): 49.77777777777778..63.888888888888886
  HPD interval (0.9999): 49.111111111111114..64.55555555555556

  variable : sample_diff
  mean: 3.5729175555555557
  HPD interval (0.9): 0.08888888888888889..7.3
  HPD interval (0.99): 0.08888888888888889..10.88888888888889
  HPD interval (0.999): 0.08888888888888889..13.422222222222222
  HPD interval (0.9999): 0.08888888888888889..14.688888888888888

  variable : p
  mean: 0.0017700000000000012

  * Using resample (and 10000 samples)

  variable : sample_placebo
  mean: 56.88871000000398
  HPD interval (0.9): 51.6..61.3
  HPD interval (0.99): 49.7..64.6
  HPD interval (0.999): 48.1..66.5
  HPD interval (0.9999): 47.2..67.8

  variable : sample_drug
  mean: 56.852622222226195
  HPD interval (0.9): 51.55555555555556..61.888888888888886
  HPD interval (0.99): 49.0..64.77777777777777
  HPD interval (0.999): 47.22222222222222..66.44444444444444
  HPD interval (0.9999): 45.77777777777778..67.88888888888889

  variable : sample_diff
  mean: 3.441450000000239
  HPD interval (0.9): 0..7.133333333333334
  HPD interval (0.99): 0..11.088888888888889
  HPD interval (0.999): 0..13.522222222222222
  HPD interval (0.9999): 0..18.11111111111111

  variable : p
  mean: 0.0026000000000001833

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define placebo '(54 51 58 44 55 52 42 47 58 46))
(define drug    '(54 73 53 70 73 68 52 65 65))
(define placebo_mean (* 1.0 (avg placebo)))
(define drug_mean    (* 1.0 (avg drug)))
(define diff_mean (- drug_mean placebo_mean))
(show2 "placebo_mean:" placebo_mean "drug_mean:" drug_mean"diff_mean:" diff_mean)
; Collect data and labels
(define all (append placebo drug))
(define labels (flatten (append (rep (length placebo) 0) (rep (length drug) 1))))
(define len (length labels))
(define placebo_len (length placebo))
(show "all:" all)
(show "labels:" labels)
(newline)

(define (model use-resample?)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define sample (if use-resample? (resample-all all) (shuffle all)))

   ; The first placebo_len elements represents placebos, the rest represents drugs
   (define sample_placebo (avg (take sample placebo_len)))
   (define sample_drug (avg (drop sample placebo_len)))
   (define sample_diff (abs (- sample_drug sample_placebo)))
    
   ; Did we see as great difference between the sample means?
   (define p (>= sample_diff diff_mean))
    
   (list (* 1.0 sample_placebo)
         (* 1.0 sample_drug)
         (* 1.0 sample_diff)
         p
         )
   )
)

(for ([use-resample? '(#f #t)])
  (show "use-resample?" use-resample?)
  (show-marginals (model use-resample?)
                  (list  "sample_placebo"
                         "sample_drug"
                         "sample_diff"
                         "p"
                         )
                  #:num-samples (if use-resample? 10000 100000)
                  #:truncate-output 5
                  #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  #:hpd-interval (list 0.9 0.99 0.999 0.9999)
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ; #:burn 0
                  ; #:thin 0
                  )
)

