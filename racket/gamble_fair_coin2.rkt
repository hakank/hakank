#| 

  Fair coin in Racket/Gamble 

  From Statistics101 (Resample Stats)
  http://www.statistics101.net/QuickReference.pdf
  Page 46
  """
  Say that you flipped the coin ten times and got seven heads. Is the coin fair? 
  Your research hypothesis is that the coin is unfair. Therefore, the null hypothesis, 
  which is the opposite of the research hypothesis, and which you are hoping to
  contradict, is that the coin is fair. You then choose a probability criterion which you consider so
  low that if the computed probability were equal to or less than the criterion then it would be
  reasonable to say that the sample could not have come from the benchmark universe. Say you
  choose 0.05. Then you would determine the probability of seven heads out of ten throws from
  that universe and compare it to your criterion.
  ...
  The result comes in around 0.12. Since this is greater than your criterion of 0.05, 
  you must conclude that seven heads out of ten is not sufficiently unusual for a fair 
  coin, so you cannot reject the null hypothesis.
  """




  The probability of getting 7 heads in 10 tosses with a fair coin (p) is about 11.7%,
  so this does not indicate an unfair coin.

  However, if we got 0, 1, 9, or 10 head this would be surprising for a fair coin.
  Note that taken these cases together, i.e. the probability of getting 
  either 0, 1, 9,or 10 coins (p2) is about 2.15% which is not _very_ uncommon.
  

  This is a port of my WebPPL model fair_coin2.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

#|

  var : s
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
  Credible interval (0.84): 3..7
  Percentiles:
  (0.01 2)
  (0.025 2)
  (0.1 3)
  (0.05 2)
  (0.25 4)
  (0.5 5)
  (0.75 6)
  (0.84 6)
  (0.9 7)
  (0.95 7)
  (0.975 8)
  (0.99 8)
  (0.999 9)

  var : p
  #f: 113/128 (0.8828125)
  #t: 15/128 (0.1171875)
  mean: 15/128 (0.1171875)

  var : p2
  #f: 501/512 (0.978515625)
  #t: 11/512 (0.021484375)
  mean: 11/512 (0.021484375)

  var : surprise
  0: 127/512 (0.248046875)
  2.041241452319315: 105/512 (0.205078125)
  -2.041241452319315: 105/512 (0.205078125)
  -4.364357804719847: 15/128 (0.1171875)
  4.364357804719847: 15/128 (0.1171875)
  -15/2: 45/1024 (0.0439453125)
  15/2: 45/1024 (0.0439453125)
  -40/3: 5/512 (0.009765625)
  40/3: 5/512 (0.009765625)
  mean: 0.0
  Credible interval (0.84): -4.364357804719847..4.364357804719847
  Percentiles:
  (0.01 -15/2)
  (0.025 -15/2)
  (0.1 -4.364357804719847)
  (0.05 -15/2)
  (0.25 -2.041241452319315)
  (0.5 0)
  (0.75 2.041241452319315)
  (0.84 2.041241452319315)
  (0.9 4.364357804719847)
  (0.95 4.364357804719847)
  (0.975 15/2)
  (0.99 15/2)
  (0.999 40/3)


|#
(define (model)
  (enumerate
   
   (define sample (repeat (lambda() (bernoulli 1/2)) 10))
   (define s (sum sample))
   (define stdev (stddev sample))
   (define p (= s 7))
   (define p2 (or (<= s 1) (>= s 9)))
   (define surprise (if (not (= stdev 0))
                        (/ (- s 5) stdev)
                        0))
   (list s
         p
         p2
         surprise
         )

   )
)

(displayln "Model 1")
(show-marginals (model)
                (list  "s"
                       "p"
                       "p2"
                       "surprise"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                #:show-percentiles? #t
                )

#|
 Using binomial instead.

 We get - unsurprisingly - almost exactly the same result (it differs in the last decials).

  var : s
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
  Credible interval (0.84): 3..7
  Percentiles:
  (0.01 1)
  (0.025 2)
  (0.1 3)
  (0.05 2)
  (0.25 4)
  (0.5 5)
  (0.75 6)
  (0.84 7)
  (0.9 7)
  (0.95 7)
  (0.975 8)
  (0.99 8)
  (0.999 9)

  var : p
  #t: 0.9453125
  #f: 0.0546875
  mean: 0.9453125

  var : p2
  #f: 0.978515625
  #t: 0.021484375000000003
  mean: 0.021484375000000003

|#
(define (model2)
  (enumerate
   
   (define s (binomial 10 1/2))
   (define p (= s 7))
   (define p2 (or (<= s 1) (>= s 9)))
   (list s
         p
         p2
    )
   )
)
(displayln "\nModel 2")
(show-marginals (model2)
                (list  "s"
                       "p"
                       "p2"
                       )
                #:credible-interval 0.84
                #:show-percentiles? #t
                )


