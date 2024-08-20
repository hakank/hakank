#| 

  Hypothesis testing in Racket Gamble.


  From the AgenaRisk model Tutorial/Hypothesis Testing
  Comparison of two materials A and B which has different number of tests of faultyness:

  - A was tested in 200 cases where 10 was faulty
  - B was tested in 100 cases where 9 was fault.

  Is A better then B?

  (Note: This is - yet another variant of an - A/B test.)

  var : prob_a_is_faulty
  mean: 0.05630673282942747
  Credible interval (0.84): 0.03405823863207215..0.07387419193187542

  var : prob_b_is_faulty
  mean: 0.09981356993231992
  Credible interval (0.84): 0.057864542127546104..0.1356617885682853

  var : a<b
  #t: 0.9236999999999999
  #f: 0.07630000000000003
  mean: 0.9236999999999999
  Credible interval (0.84): 1..1

  var : hypothesis
  A_better_than_B: 0.9236999999999999
  A_not_better_than_B: 0.07630000000000003


  This is ported from my WebPPL model hypothesis_testing.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (hypothesis-testing)
  
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler
   
   (define a_tests 200)
   (define b_tests 100)
   
   (define prob_a_is_faulty (uniform 0 1))
   (define prob_b_is_faulty (uniform 0 1))
   
   (define a_faults (binomial a_tests prob_a_is_faulty))
   (define b_faults (binomial b_tests prob_b_is_faulty))
   
   (define hypothesis
     (if (< prob_a_is_faulty prob_b_is_faulty)
         "A_better_than_B"
         "A_not_better_than_B"))
   
   (observe/fail (= a_faults 10))
   (observe/fail (= b_faults 9))

   (list prob_a_is_faulty
         prob_b_is_faulty        
         (< prob_a_is_faulty prob_b_is_faulty)
         hypothesis
         )

   )
  )

(show-marginals (hypothesis-testing)
                (list "prob_a_is_faulty"
                      "prob_b_is_faulty"
                      "a<b"
                      "hypothesis"
                      )
                #:num-samples 10000
                #:truncate-output 1
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                #:credible-interval2 0.84                
                
                )

