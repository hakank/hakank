#| 

  Pareto type II distribution in Racket/Gamble 

  From Mathematica ParetoDistribution
  ParetoDistribution[k,alpha,mu]
  represents a Pareto type II distribution with 
  - minimum parameter k, 
  - shape parameter alpha 
  - location parameter mu

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


#|

  variable : d
  3.863769584762636: 1.0000000000019164e-5
  2.9666233743467423: 1.0000000000019164e-5
  1.5224498812465352: 1.0000000000019164e-5
  1.904679572196017: 1.0000000000019164e-5
  3.1198875237935955: 1.0000000000019164e-5
  ...
  2.6224883082446215: 1.0000000000019164e-5
  1.760051383291585: 1.0000000000019164e-5
  2.450167277752021: 1.0000000000019164e-5
  1.8834125227989915: 1.0000000000019164e-5
  1.5457955003278352: 1.0000000000019164e-5
  mean: 2.4993382846040535
  HPD interval (0.84): 1.500003964500889..3.202932266408794
  HPD interval (0.9): 1.500003964500889..3.8226955652128014
  HPD interval (0.99): 1.500003964500889..8.70373892027481
  HPD interval (0.99999): 1.500003964500889..126.5102940639509
  Percentiles:
  (0.01 1.5064101511339634)
  (0.025 1.5171496414619816)
  (0.1 1.5720266194684234)
  (0.05 1.5346310765879316)
  (0.25 1.7010462334987566)
  (0.5 2.0211280045679407)
  (0.75 2.691177496585534)
  (0.84 3.2029213465081474)
  (0.9 3.8226763088562015)
  (0.95 4.947647679068712)
  (0.975 6.361440416934013)
  (0.99 8.697086094219586)
  (0.999 19.60703320110644)
  Histogram:
1.5    :     1 # (1e-5  / 0    )
2.83   : 77952 ################################################################################ (0.779 / 1e-5 )
4.16   : 14100 ############### (0.141 / 0.779)
5.49   :  4154 ##### (0.041 / 0.920)
6.82   :  1734 ## (0.017 / 0.962)
8.149  :   837 # (0.008 / 0.979)
9.479  :   430 # (0.004 / 0.987)
10.809 :   233 # (0.002 / 0.992)
12.139 :   167 # (0.001 / 0.994)
13.469 :    83 # (0.000 / 0.996)
14.799 :    83 # (0.000 / 0.996)
16.129 :    40 # (0.000 / 0.997)
17.459 :    42 # (0.000 / 0.998)
18.789 :    29 # (0.000 / 0.998)
20.119 :    19 # (0.000 / 0.998)
21.448 :    11 # (0.000 / 0.999)
22.778 :    11 # (0.000 / 0.999)
24.108 :    12 # (0.000 / 0.999)
25.438 :     9 # (9e-5  / 0.999)
26.768 :     8 # (8e-5  / 0.999)
28.098 :     8 # (8e-5  / 0.999)
29.428 :     9 # (9e-5  / 0.999)
30.758 :     4 # (4e-5  / 0.999)
32.088 :     2 # (2e-5  / 0.999)
33.418 :     2 # (2e-5  / 0.999)
34.747 :     4 # (4e-5  / 0.999)
36.077 :     1 # (1e-5  / 0.999)
37.407 :     1 # (1e-5  / 0.999)
38.737 :     1 # (1e-5  / 0.999)
40.067 :     1 # (1e-5  / 0.999)
41.397 :     0  (0     / 0.999)
42.727 :     0  (0     / 0.999)
44.057 :     4 # (4e-5  / 0.999)
45.387 :     0  (0     / 0.999)
46.716 :     0  (0     / 0.999)
48.046 :     0  (0     / 0.999)
49.376 :     0  (0     / 0.999)
50.706 :     0  (0     / 0.999)
52.036 :     0  (0     / 0.999)
53.366 :     1 # (1e-5  / 0.999)
54.696 :     0  (0     / 0.999)
56.026 :     0  (0     / 0.999)
57.356 :     2 # (2e-5  / 0.999)
58.686 :     1 # (1e-5  / 0.999)
60.015 :     0  (0     / 0.999)
61.345 :     0  (0     / 0.999)
62.675 :     0  (0     / 0.999)
64.005 :     0  (0     / 0.999)
65.335 :     0  (0     / 0.999)
66.665 :     0  (0     / 0.999)
67.995 :     0  (0     / 0.999)
69.325 :     0  (0     / 0.999)
70.655 :     0  (0     / 0.999)
71.985 :     0  (0     / 0.999)
73.314 :     0  (0     / 0.999)
74.644 :     0  (0     / 0.999)
75.974 :     0  (0     / 0.999)
77.304 :     1 # (1e-5  / 0.999)
78.634 :     0  (0     / 0.999)
79.964 :     0  (0     / 0.999)
81.294 :     0  (0     / 0.999)
82.624 :     0  (0     / 0.999)
83.954 :     0  (0     / 0.999)
85.283 :     0  (0     / 0.999)
86.613 :     0  (0     / 0.999)
87.943 :     0  (0     / 0.999)
89.273 :     1 # (1e-5  / 0.999)
90.603 :     1 # (1e-5  / 0.999)
91.933 :     0  (0     / 0.999)
93.263 :     0  (0     / 0.999)
94.593 :     0  (0     / 0.999)
95.923 :     0  (0     / 0.999)
97.253 :     0  (0     / 0.999)
98.582 :     0  (0     / 0.999)
99.912 :     0  (0     / 0.999)
101.242:     0  (0     / 0.999)
102.572:     0  (0     / 0.999)
103.902:     0  (0     / 0.999)
105.232:     0  (0     / 0.999)
106.562:     0  (0     / 0.999)
107.892:     0  (0     / 0.999)
109.222:     0  (0     / 0.999)
110.552:     0  (0     / 0.999)
111.881:     0  (0     / 0.999)
113.211:     0  (0     / 0.999)
114.541:     0  (0     / 0.999)
115.871:     0  (0     / 0.999)
117.201:     0  (0     / 0.999)
118.531:     0  (0     / 0.999)
119.861:     0  (0     / 0.999)
121.191:     0  (0     / 0.999)
122.521:     0  (0     / 0.999)
123.851:     0  (0     / 0.999)
125.18 :     0  (0     / 0.999)

variable : p
#f: 0.9903100000038509
#t: 0.009690000000018613
mean: 0.009690000000018613
  Histogram:
#f: 99015 ################################################################################ (0.990 / 0    )
#t:   985 # (0.009 / 0.990)


|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define k 2)
   (define alpha 3)
   (define mu 1.5)
   
   (define d (pareto2_dist k alpha mu))

   (define p (>= d (pareto2_quantile k alpha mu 0.99)))
   
   (list d
         p)
     
   )
)

(show-marginals (model)
                (list  "d"
                       "p"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.90 0.99 0.99999)
                #:show-histogram? #t
                #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )



