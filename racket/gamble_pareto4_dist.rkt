#| 

  Pareto type IV distribution in Racket/Gamble 

  From Mathematica ParetoDistribution.
  ParetoDistribution[k,alpha,gamma,mu]
  represents a Pareto type IV distribution with 
  - minimum parameter k, 
  - shape parameter alpha 
  - shape parameter gamma
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
  3.4601966473077588: 2.0000000000038325e-5
  1.9100297289703154: 2.0000000000038325e-5
  1.565071951139653: 2.0000000000038325e-5
  7.177809834433444: 1.0000000000019162e-5
  1.7185565889586383: 1.0000000000019162e-5
  ...
  1.9445354552284941: 1.0000000000019162e-5
  3.0362813890133005: 1.0000000000019162e-5
  1.8658256798236472: 1.0000000000019162e-5
  7.082943678649133: 1.0000000000019162e-5
  1.7168292157202072: 1.0000000000019162e-5
  mean: 2.5061778582873893
  HPD interval (0.84): 1.5000007799833024..3.1696761522021926
  HPD interval (0.9): 1.5000007799833024..3.796900908907433
  HPD interval (0.99): 1.5000007799833024..8.812800929093012
  HPD interval (0.99999): 1.5000007799833024..92.98990498806182
  Percentiles:
  (0.01 1.5061349598943639)
  (0.025 1.5164034294712305)
  (0.1 1.570760302252511)
  (0.05 1.5339132807592297)
  (0.25 1.7007908027142316)
  (0.5 2.0189519786455556)
  (0.75 2.6675950294944277)
  (0.84 3.1695027332919867)
  (0.9 3.7967280628320887)
  (0.95 4.92955651447804)
  (0.975 6.377597848113591)
  (0.99 8.810532126636124)
  (0.999 18.94973735758577)
  Histogram:
1.5   :     1 # (1e-5  / 0    )
2.473 : 69722 ################################################################################ (0.697 / 1e-5 )
3.447 : 17331 #################### (0.173 / 0.697)
4.42  :  6250 ######## (0.062 / 0.870)
5.393 :  2762 #### (0.027 / 0.933)
6.366 :  1426 ## (0.014 / 0.960)
7.34  :   834 # (0.008 / 0.974)
8.313 :   487 # (0.004 / 0.983)
9.286 :   317 # (0.003 / 0.988)
10.26 :   227 # (0.002 / 0.991)
11.233:   149 # (0.001 / 0.993)
12.206:    94 # (0.000 / 0.995)
13.18 :    83 # (0.000 / 0.996)
14.153:    57 # (0.000 / 0.996)
15.126:    50 # (0.000 / 0.997)
16.099:    37 # (0.000 / 0.997)
17.073:    29 # (0.000 / 0.998)
18.046:    21 # (0.000 / 0.998)
19.019:    25 # (0.000 / 0.998)
19.993:    20 # (0.000 / 0.999)
20.966:    12 # (0.000 / 0.999)
21.939:     4 # (4e-5  / 0.999)
22.913:    14 # (0.000 / 0.999)
23.886:     3 # (3e-5  / 0.999)
24.859:     2 # (2e-5  / 0.999)
25.832:     3 # (3e-5  / 0.999)
26.806:     8 # (8e-5  / 0.999)
27.779:     2 # (2e-5  / 0.999)
28.752:     1 # (1e-5  / 0.999)
29.726:     1 # (1e-5  / 0.999)
30.699:     1 # (1e-5  / 0.999)
31.672:     1 # (1e-5  / 0.999)
32.646:     3 # (3e-5  / 0.999)
33.619:     0  (0     / 0.999)
34.592:     2 # (2e-5  / 0.999)
35.565:     2 # (2e-5  / 0.999)
36.539:     4 # (4e-5  / 0.999)
37.512:     1 # (1e-5  / 0.999)
38.485:     0  (0     / 0.999)
39.459:     2 # (2e-5  / 0.999)
40.432:     0  (0     / 0.999)
41.405:     2 # (2e-5  / 0.999)
42.378:     1 # (1e-5  / 0.999)
43.352:     2 # (2e-5  / 0.999)
44.325:     0  (0     / 0.999)
45.298:     0  (0     / 0.999)
46.272:     0  (0     / 0.999)
47.245:     0  (0     / 0.999)
48.218:     1 # (1e-5  / 0.999)
49.192:     0  (0     / 0.999)
50.165:     0  (0     / 0.999)
51.138:     0  (0     / 0.999)
52.111:     0  (0     / 0.999)
53.085:     1 # (1e-5  / 0.999)
54.058:     0  (0     / 0.999)
55.031:     0  (0     / 0.999)
56.005:     0  (0     / 0.999)
56.978:     0  (0     / 0.999)
57.951:     0  (0     / 0.999)
58.925:     0  (0     / 0.999)
59.898:     0  (0     / 0.999)
60.871:     0  (0     / 0.999)
61.844:     0  (0     / 0.999)
62.818:     0  (0     / 0.999)
63.791:     1 # (1e-5  / 0.999)
64.764:     1 # (1e-5  / 0.999)
65.738:     1 # (1e-5  / 0.999)
66.711:     0  (0     / 0.999)
67.684:     0  (0     / 0.999)
68.657:     0  (0     / 0.999)
69.631:     0  (0     / 0.999)
70.604:     0  (0     / 0.999)
71.577:     0  (0     / 0.999)
72.551:     1 # (1e-5  / 0.999)
73.524:     0  (0     / 0.999)
74.497:     0  (0     / 0.999)
75.471:     0  (0     / 0.999)
76.444:     0  (0     / 0.999)
77.417:     0  (0     / 0.999)
78.39 :     0  (0     / 0.999)
79.364:     0  (0     / 0.999)
80.337:     0  (0     / 0.999)
81.31 :     0  (0     / 0.999)
82.284:     0  (0     / 0.999)
83.257:     0  (0     / 0.999)
84.23 :     0  (0     / 0.999)
85.204:     0  (0     / 0.999)
86.177:     0  (0     / 0.999)
87.15 :     0  (0     / 0.999)
88.123:     0  (0     / 0.999)
89.097:     0  (0     / 0.999)
90.07 :     0  (0     / 0.999)
91.043:     0  (0     / 0.999)
92.017:     0  (0     / 0.999)

  variable : p
  #f: 0.9896400000038464
  #t: 0.010360000000019859
  mean: 0.010360000000019859
  Histogram:
#f: 98995 ################################################################################ (0.989 / 0    )
#t:  1005 # (0.010 / 0.989)


|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define k 2)
   (define alpha 3)
   (define gamma 1)
   (define mu 1.5)
   
   (define d (pareto4_dist k alpha gamma mu))

   (define p (>= d (pareto4_quantile k alpha gamma mu 0.99)))
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
                #:hpd-interval (list 0.84 0.9 0.99 0.99999)
                #:show-histogram? #t
                #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

