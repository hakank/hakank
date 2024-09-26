#| 

  Negative Binomial test in Racket Gamble.

  From BLOG documentation:
  """
  NegativeBinomial distribution generates the number of failures until the rth success 
  in a sequence of independent Bernoulli trials each with probability of success p.

  Example: The following code defines a random function symbol x distributed according to a
  Negative Binomial distribution with probability of success p = 0.8 and number of failures r = 2.
  random Integer x ~ NegativeBinomial(2, 0.8);
  """


  https://stattrek.com/probability-distributions/negative-binomial.aspx
  """
  Bob is a high school basketball player. He is a 70% free throw shooter. That means 
  his probability of making a free throw is 0.70. During the season, what is the 
  probability that Bob makes his third free throw on his fifth shot?

  Solution: This is an example of a negative binomial experiment. The probability of 
  success (P) is 0.70, the number of trials (x) is 5, and the number of 
  successes (r) is 3.

  To solve this problem, we enter these values into the negative binomial formula.
  
  b*(x; r, P) = x-1Cr-1 * Pr * Qx - r
  b*(5; 3, 0.7) = 4C2 * 0.73 * 0.32
  b*(5; 3, 0.7) = 6 * 0.343 * 0.09 = 0.18522
  
  Thus, the probability that Bob will make his third successful free throw on his 
  fifth shot is 0.18522.
  """

  Gamble does not have an negative binomial distribution, so we have to roll 
  our own.

  * Using (negative_binomial 3 0.7) and enumerate (with a limit)

  var : y
  0: 0.34300342843668685
  1: 0.3087030855930182
  2: 0.18522185135581096
  3: 0.09261092567790549
  4: 0.04167491655505745
  5: 0.01750346495312413
  6: 0.0070013859812496455
  7: 0.0027005345927677217
  8: 0.0010127004722878948
  9: 0.0003713235065055633
  10: 0.00013367646234200225
  11: 4.739438210307371e-5
  12: 1.5312031140993005e-5
  mean: 1.2855939768981777
  Credible interval (0.94): 0..4
  Credible-interval2 (0.94): 0..4 (ps: (0.030000000000000027 0.97))
  Histogram:
   0: 3382
   1: 3184
   2: 1889
   3: 913 
   4: 400 
   5: 140 
   6: 58  
   7: 24  
   8: 13  
   9: 6   
  10: 2   

  var : p
  #f: 0.8147781486441892
  #t: 0.18522185135581096
  mean: 0.18522185135581096
  Credible interval (0.94): 0..1
  Credible-interval2 (0.94): #f..#t (ps: (0.030000000000000027 0.97))
  Histogram:
  #f: 8113
  #t: 1889


  This is a port of my WebPPL model  negative_binomial_test.wppl (WebPPL does not have a
  native negative binomial distribution either)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  
  (enumerate
   #:limit 1e-05
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define limit 20)
   ; This works with enumerate
   (define y (negative_binomial 3 0.7))

   ; This does not work with enumerate
   ; (define y (negative_binomial_dist 3 0.7)) 
   
   (define p (= y 2))
   
   (list y
         p
         )
         
   )
  )

(show-marginals (model)
                (list "y"
                      "p"
                      )
                #:num-samples 10000
                ; #:truncate-output 10
                ; #:skip-marginals? #t
                #:credible-interval 0.94
                #:credible-interval2 0.94
                #:show-histogram? #t
                ; #:show-percentiles? #t                
                )
