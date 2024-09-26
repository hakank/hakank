#| 

  Comparing two proportions in Racket/Gamble 

  From http://www.statistics101.net/statistics101web_000007.htm
  """
  From CliffsQuickReview Statistics, Example 16, Page 92: 
  A swimming school wants to determine whether a recently 
  hired instructor is working out. Sixteen out of 25 of 
  Instructor A's students passed the lifeguard certification 
  test on the first try. In comparison, 57 out of 72 of more 
  experienced Instructor B's students passed the test on the 
  first try. Is Instructor A's success rate worse than 
  Instructor B's? Use alpha = 0.10. 
 
  Null hypothesis: A  s rate is >= B  s rate 
  Alternate hypothesis: A  s rate is < B  s rate 
  This is a one-tailed test. 
  """


  (a 16/25: 16/25 b: 57/72 19/24)
  Model 1
  var : sample_a
  mean: 64.31599999999996

  var : sample_b
  mean: 79.49166666666659

  var : p
  mean: 0.06599999999999999


  Model 2
  var : rateA
  mean: 0.6260899513645067

  var : rateB
  mean: 0.7833945544662274

  var : p
  mean: 0.07100000000000004

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(show2 "a 16/25:" 16/25 "b: 57/72" 57/72)
(define a (append (rep 16 "passed") (rep 9 "failed")))
(define b (append (rep 57 "passed") (rep 15 "failed")))

(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define sample_a (* 100 (/ (count-occurrences-eq "passed" (resample 25 a)) 25)))
   (define sample_b (* 100 (/ (count-occurrences-eq "passed" (resample 72 b)) 72)))
   (define p (>= sample_a sample_b))

   (list sample_a
         sample_b
         p
         )
   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list  "sample_a"
                       "sample_b"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

#|
  Here we look for the the rates for a and b in binomial(p,n),
  and check the difference of the two rates.

|#

(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define nA 25) ; number of trial for A    
   (define nB 72) ; number of trial for B

   (define rateA (beta 1 1))
   (define sA (binomial nA rateA))
   (observe/fail (= sA 16)) ; number of successes for A
    
   (define rateB (beta 1 1))
   (define sB (binomial nB rateB))
   (observe/fail (= sB 57)) ; number of successes for B

   (define p (> rateA rateB))
    
   (list rateA
         rateB
         p
         )
   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "rateA"
                       "rateB"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


