#| 

  Negative binomial coins II in Racket/Gamble 
  From Mathematica (NegativeBinomialDistribution)
  """
  A coin was flipped 10 times and the 8^th head occurred at 
  the 10^th flip. Find the probability of such an event if the coin is fair:

    fairD = NegativeBinomialDistribution(8, 1/2);
    Probability(x == 2, x fairD))
    -> 
      9/256 (0.0351563)

  Assuming the coin may not be fair, find the most likely value for p:
    D = NegativeBinomialDistribution(8, p);
    FindMaximum(PDF(D, 2), (p, 0.9))
    -> 
    (0.241592, (p -> 0.8))
  """

  variable : fair
  6: 0.10552000000000002
  7: 0.10376000000000002
  8: 0.09863000000000001
  5: 0.09541000000000001
  9: 0.08739000000000001
  ...
  31: 3.0000000000000004e-5
  30: 2.0000000000000005e-5
  32: 1.0000000000000003e-5
  33: 1.0000000000000003e-5
  34: 1.0000000000000003e-5
  mean: 8.002310000000001

  variable : p
  #f: 0.9641899999999999
  #t: 0.03581000000000001
  mean: 0.03581000000000001

  (negative_binomial_mean 8 1/2):: 8
  ((negative_binomial_cdf 8 1/2 2): 9/256 0.03515625)

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

   (define fair (negative_binomial 8 1/2))
   (define p (= fair 2))
   
   (list fair
         p
         )
   )
)

(show-marginals (model)
                (list  "fair"
                       "p"
                     )
                    #:num-samples 100000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )

(newline)
(let* ([m 8]
       [p 1/2]
       [v (negative_binomial_pdf m p 2)])
  (show "(negative_binomial_mean 8 1/2):" (negative_binomial_mean m p))
  (show2 "(negative_binomial_cdf 8 1/2 2):" v (* 1.0 v) )
  )
(newline)

