#| 

  Political survey in Racket/Gamble 

  From "Resampling Stats Illustrations" (http://www.statistics101.net/PeterBruce_05-illus.pdf)
  Page 27
  """
  A confidence interval for a political survey
  (program “bush”)
  One of the Gallup polls for the 1988 U. S. presidential election
  showed 840 (56%) for Bush and 660 for Dukakis (a total of 1500
  voters). Estimate bounds on the percentage of the entire electorate
  that favors Bush.

  Put another way, we would like to learn how variable our sample
  result is. How much might one sample differ from another?
  If we had unlimited time and money, we could take additional
  surveys of 1500 each to see how much they differ from one an-
  other. Lacking the ability to go back to the actual universe and
  draw more samples from it, we do the next best thing — we create
  a hypothetical “bootstrap” universe based on the sample data, and
  then draw samples from it.
  -> 
  interval = 0.539 to 0.58
  """

  The answer of the question is the HPDs for bush_pct:
    HPD interval (0.9): 53.86666666666667..58.0
    HPD interval (0.95): 53.6..58.53333333333333
    HPD interval (0.99): 52.666666666666664..59.13333333333333


  variable : bush
  mean: 840.0620000000009
  HPD interval (0.9): 808..870
  HPD interval (0.95): 804..878
  HPD interval (0.99): 790..887

  variable : dukakis
  mean: 659.9380000000004
  HPD interval (0.9): 627..689
  HPD interval (0.95): 622..696
  HPD interval (0.99): 612..709

  variable : diff
  mean: 180.124
  HPD interval (0.9): 116..240
  HPD interval (0.95): 108..256
  HPD interval (0.99): 80..274

  variable : bush_pct
  mean: 56.00413333333337
  HPD interval (0.9): 53.86666666666667..58.0
  HPD interval (0.95): 53.6..58.53333333333333
  HPD interval (0.99): 52.666666666666664..59.13333333333333

  variable : dukakis_pct
  mean: 43.99586666666667
  HPD interval (0.9): 41.8..45.93333333333333
  HPD interval (0.95): 41.46666666666667..46.4
  HPD interval (0.99): 40.8..47.266666666666666

  variable : bush_wins
  mean: 1.0

  variable : dukakis_wins
  mean: 0 (0.0)

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

   (define num_votes 1500)
   (define votes (repeat (lambda () (categorical-vw2 (vector 840 660) (vector "Bush" "Dukakis"))) num_votes))
   (define bush (count-occurrences-eq "Bush" votes))
   (define bush_pct (* (/ bush num_votes) 100))
   (define dukakis (count-occurrences-eq "Dukakis" votes))
   (define dukakis_pct (* (/ dukakis num_votes) 100))
   (define diff (- bush dukakis))
   (define bush_wins (> bush dukakis))
   (define dukakis_wins (> dukakis bush))

   (list bush
         dukakis
         diff
         (* 1.0 bush_pct)
         (* 1.0 dukakis_pct)
         bush_wins
         dukakis_wins
         )

   )
)

(show-marginals (model)
                (list  "bush"
                       "dukakis"
                       "diff"
                       "bush_pct"
                       "dukakis_pct"
                       "bush_wins"
                       "dukakis_wins"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.9 0.95 0.99)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


