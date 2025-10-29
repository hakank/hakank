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

variable : y
0: 0.34300000000001857
1: 0.30870000000001674
2: 0.18522000000001007
3: 0.09261000000000506
4: 0.04167450000000226
5: 0.01750329000000095
6: 0.007001316000000373
7: 0.002700507600000145
8: 0.0010126903500000536
9: 0.0003713197950000215
10: 0.0001336751262000072
11: 4.739390838000274e-5
12: 1.658786793300094e-5
13: 5.741954284500352e-6
14: 1.968670040400107e-6
15: 6.693478137360404e-7
16: 2.2590488713591295e-7
17: 7.574457980439534e-8
18: 2.524819326813166e-8
19: 8.371769346801594e-9
20: 2.762683884444508e-9
21: 9.077389906031961e-10
22: 2.970782151065004e-10
23: 9.687333101298983e-11
24: 3.148383257922186e-11
25: 1.0200761755667865e-11
26: 3.295630721061881e-12
27: 1.0619254545643997e-12
28: 3.413331818242714e-13
29: 1.0663719404578974e-13
mean: 1.2857142857127064
Credible interval (0.94): 0..4
Credible-interval2 (0.94): 0..4 (ps: (0.030000000000000027 0.97))
Histogram:
 0: 3442 ################################################################################ (0.344 / 0    )
 1: 3018 ####################################################################### (0.301 / 0.344)
 2: 1883 ############################################ (0.188 / 0.646)
 3:  931 ###################### (0.093 / 0.834)
 4:  452 ########### (0.045 / 0.927)
 5:  156 #### (0.015 / 0.972)
 6:   78 ## (0.007 / 0.988)
 7:   21 # (0.002 / 0.996)
 8:   14 # (0.001 / 0.998)
 9:    4 # (0.000 / 0.999)
10:    1 # (0.000 / 0.999)

variable : p
#f: 0.8147799999999901
#t: 0.18522000000001007
mean: 0.18522000000001007
Credible interval (0.94): 0..1
Credible-interval2 (0.94): #f..#t (ps: (0.030000000000000027 0.97))
Histogram:
#f: 8117 ################################################################################ (0.811 / 0    )
#t: 1883 ################### (0.188 / 0.811)



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
   #:limit 1e-15
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


#|
  Using 0/1 (bernoulli) representation instead

  Model2
  variable : p
  #f: 0.81478
  #t: 0.18522000000000002
  mean: 0.18522000000000002
  Credible interval (0.94): 0..1
  Credible-interval2 (0.94): #f..#t (ps: (0.030000000000000027 0.97))
  Histogram:
  #f: 8158 ################################################################################ (0.815 / 0    )
  #t: 1842 ################### (0.184 / 0.815)

|#
(define (model2)
  
  (enumerate
   ; #:limit 1e-05
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 5)
   (define hits (for/list ([i 5]) (bernoulli 0.7)))
 
   (define p (and (eq? (sum hits) 3) (eq? (last hits) 1)))
   
   (list p
         )
         
   )
  )

(displayln "\nModel2")
(show-marginals (model2)
                (list "p"
                      )
                #:num-samples 10000
                ; #:truncate-output 10
                ; #:skip-marginals? #t
                #:credible-interval 0.94
                #:credible-interval2 0.94
                #:show-histogram? #t
                ; #:show-percentiles? #t                
                )
