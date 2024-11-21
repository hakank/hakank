#| 

  Mean and stdev for 10 flips of a fair coin in Racket/Gamble 

  From http://www.statistics101.net/statistics101web_000007.htm
  """
  From CliffsQuickReview Statistics, p. 47, example 7 
  What is the mean and standard deviation for a 
  binomial probability distribution for 10 flips 
  of a fair coin? 
  Calculated result using binomial formula:  
  mean = 5, standard deviation = 1.58  
  """

  variable : s
  5: 63/256 (0.24609375)
  4: 105/512 (0.205078125)
  6: 105/512 (0.205078125)
  3: 15/128 (0.1171875)
  7: 15/128 (0.1171875)
  2: 45/1024 (0.0439453125)
  8: 45/1024 (0.0439453125)
  1: 5/512 (0.009765625)
  9: 5/512 (0.009765625)
  0: 1/1024 (0.0009765625)
  10: 1/1024 (0.0009765625)
  mean: 5 (5.0)
  Min: 0 Mean: 4.9958 Max: 10 Variance: 2.50978236 Stddev: 1.5842292637115374
  HPD interval (0.84): 3..7
  HPD interval (0.9): 2..7
  HPD interval (0.95): 2..8
  HPD interval (0.99): 1..9

  And using (binomial 10 1/2):
  variable : s2
  5: 0.24609375
  4: 0.20507812500000006
  6: 0.20507812500000006
  3: 0.11718749999999997
  7: 0.11718749999999997
  2: 0.0439453125
  8: 0.0439453125
  1: 0.009765625000000002
  9: 0.009765625000000002
  0: 0.0009765625
  10: 0.0009765625
  mean: 5.0
  Min: 0 Mean: 5.0057 Max: 10 Variance: 2.47686751 Stddev: 1.573806693974835
  HPD interval (0.84): 3..7
  HPD interval (0.9): 2..7
  HPD interval (0.95): 2..8
  HPD interval (0.99): 1..8



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define s (sum (resample 10 '(0 1))))
   (list s
         )

   )
)

(show-marginals (model1)
                (list  "s"
                       "s2"
                     )
                    #:num-samples 10000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84 0.90 0.95 0.99)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define s2 (binomial 10 1/2))
   (list s2
         )

   )
)

(show-marginals (model2)
                (list  "s2"
                     )
                    #:num-samples 10000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84 0.90 0.95 0.99)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


