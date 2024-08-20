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

  * Using (negative_binomial_dist2 3 0.7) and enumerate

var : y
0: 0.3430000000000001
1: 0.3087000000000001
2: 0.18522000000000008
3: 0.09261000000000007
4: 0.04167450000000001
5: 0.017503290000000005
6: 0.007001315999999995
7: 0.0027005075999999993
8: 0.00101269035
9: 0.0003713197950000015
54: 1.3644254290393474e-27
55: 3.4179833913017743e-28
56: 8.407579724584549e-29
57: 2.0222152540177456e-29
58: 4.728027186295881e-30
59: 1.0652898074562232e-30
60: 2.281916049954993e-31
61: 4.5400930512759145e-32
62: 8.011928914016392e-33
63: 1.1445612734309028e-33
mean: 1.285714285700838
ix: 0
Credible interval (0.94): 0..4
Credible-interval2 (0.94): 0..5 (ps: (0.030000000000000027 0.97))
Histogram:
 0: 3402
 1: 3117
 2: 1816
 3: 978 
 4: 390 
 5: 183 
 6: 74  
 7: 31  
 8: 9   
 9: 6   
10: 3   
11: 2   
12: 2   

var : p
#f: 0.8147800000000004
#t: 0.18522000000000008
mean: 0.18522000000000008
ix: 0
Credible interval (0.94): 0..1
Credible-interval2 (0.94): #f..#t (ps: (0.030000000000000027 0.97))
Histogram:
#f: 8186
#t: 1816


  This is a port of my WebPPL model  negative_binomial_test.wppl (WebPPL does not have a
  native negative binomial distribution either)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


; Note: The geometric distribution is infinite, so beware!
(define (negative_binomial_dist m p)
  (for/sum ([i (range m)]) (geometric p))
)

; This version of geometric distribution has a limit of
; the recursion, which makes it work with enumerate
(define (geometric_dist2 p limit)
  (define (loop p limit c)
    (if (or (> c limit) (flip p)) 0 
        (add1 (loop p limit (add1 c))))
    )
  (loop p limit 0)  
  )


(define (negative_binomial_dist2 m p limit)
  (for/sum ([i (range m)]) (geometric_dist2 p limit))
)


(define (model)
  
  (enumerate 
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define limit 20)
   ; This works with enumerate
   (define y (negative_binomial_dist2 3 0.7 limit))

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
                #:truncate-output 10
                ; #:skip-marginals? #t
                #:credible-interval 0.94
                #:credible-interval2 0.94
                #:show-histogram? #t
                ; #:show-percentiles? #t                
                )
