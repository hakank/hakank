#| 

  Laplace distribution in Racket.Gamble 

  From Handbook on probability distributions
  page 72ff

  """
  Let U be a uniform variate. Then the algorithm is
  * V = U − 1/2
  * X = m + sigma sign(V ) log(1 − 2|V |)
  return X
  """

  See gamble_distributions.rkt and gamble_distributions_test.rkt for more on this.

  Example from Mathematica's LaplaceDistribution
  """
  The difference of flood stages between river stations A and B in a year has 
  been estimated to follow a Laplace distribution with mu==10 feet and b==3.4 feet. 
  Find the probability that the difference is greater than 15 feet: 
  D = LaplaceDistribution[10, 3.4]

  Probability[Abs[u] > 15,  u e D]
  -> 
  0.115215
  """

var : d
3.9540165463845938: 0.00010000000000000938
9.407461788538216: 0.00010000000000000938
-0.5498174289792566: 0.00010000000000000938
6.047888035534646: 0.00010000000000000938
9.26362868684396: 0.00010000000000000938
...
9.397771771741713: 0.00010000000000000938
9.015449577563455: 0.00010000000000000938
9.146677120670649: 0.00010000000000000938
11.792152493393353: 0.00010000000000000938
16.830619221573777: 0.00010000000000000938
mean: 10.064163272855767
Min: -17.54301252956244 Mean: 10.095769055688505 Max: 41.52108077352402 Variance: 23.222731132550315 Stddev: 4.818996901072911
Credible interval (0.84): 3.747105494430447..16.1806201476029
Percentiles:
(0.01 -2.9698844042012063)
(0.025 0.19284411993494288)
(0.1 4.589125159749001)
(0.05 2.4884984281739353)
(0.25 7.65487908449264)
(0.5 10.057893670070317)
(0.75 12.422874165325975)
(0.84 14.00343679092364)
(0.9 15.625070370887787)
(0.95 18.04234431821142)
(0.975 20.34134512308725)
(0.99 23.656705738209908)
(0.999 31.601405946733514)
Histogram:
-17.543: 1   
-16.201: 0   
-14.858: 2   
-13.516: 2   
-12.174: 0   
-10.831: 2   
-9.489 : 3   
-8.146 : 11  
-6.804 : 11  
-5.462 : 18  
-4.119 : 30  
-2.777 : 27  
-1.435 : 49  
-0.092 : 81  
1.25   : 102 
2.592  : 178 
3.935  : 296 
5.277  : 421 
6.62   : 601 
7.962  : 894 
9.304  : 1286
10.647 : 1786
11.989 : 1365
13.331 : 894 
14.674 : 632 
16.016 : 414 
17.358 : 287 
18.701 : 195 
20.043 : 138 
21.386 : 71  
22.728 : 74  
24.07  : 42  
25.413 : 25  
26.755 : 23  
28.097 : 11  
29.44  : 9   
30.782 : 5   
32.125 : 4   
33.467 : 4   
34.809 : 3   
36.152 : 0   
37.494 : 0   
38.836 : 0   
40.179 : 1   

var : p
#f: 0.8860999999999588
#t: 0.11390000000000758
mean: 0.11390000000000758
Min: 0 Mean: 0.1215 Max: 1 Variance: 0.10673775 Stddev: 0.3267074379318598



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

    (define mu 10)
    (define sigma 3.4)
    (define d (laplace mu sigma))

    (define p (> (abs d) 15))

    (list d
          p
    )
   )
)

(show-marginals (model)
                (list  "d"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.84
                #:show-histogram? #t
                #:show-percentiles? #t
                )


