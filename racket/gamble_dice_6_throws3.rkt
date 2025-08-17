#| 

  Dice until 6 in Racket/Gamble 

  From From Mosteller "Fifty Challenging problems in Probability"
  """
  On the average, how many times must a die be thrown until one gets a 6?

  ...
 
  In our example, p = -t, and so m = 6, as seemed obvious.
  """

  The geometric distribution is the number of failure until a success.
  So the number of times until a 6 (or any certain value) is 
    1 + (geomtric_dist 1/6)

  Here we also do a simple simulation.

variable : d
6: 0.9999999999999999
mean: 5.999999999999999

variable : v
1: 0.16811000000000004
2: 0.13838000000000003
3: 0.11546000000000001
4: 0.09538000000000002
5: 0.08031000000000002
...
58: 2.0000000000000005e-5
56: 1.0000000000000003e-5
59: 1.0000000000000003e-5
60: 1.0000000000000003e-5
62: 1.0000000000000003e-5
mean: 6.001269999999999
HPD interval (0.84): 1..11
HPD interval (0.9): 1..13
HPD interval (0.95): 1..17
HPD interval (0.99): 1..26
HPD interval (0.999): 1..38
Histogram:
 1: 16700 ################################################################################ (0.167 / 0    )
 2: 13905 ################################################################### (0.139 / 0.167)
 3: 11479 ####################################################### (0.114 / 0.306)
 4:  9658 ############################################### (0.096 / 0.420)
 5:  8131 ####################################### (0.081 / 0.517)
 6:  6591 ################################ (0.065 / 0.598)
 7:  5567 ########################### (0.055 / 0.664)
 8:  4697 ####################### (0.046 / 0.720)
 9:  3796 ################### (0.037 / 0.767)
10:  3236 ################ (0.032 / 0.805)
11:  2693 ############# (0.026 / 0.837)
12:  2230 ########### (0.022 / 0.864)
13:  1968 ########## (0.019 / 0.886)
14:  1656 ######## (0.016 / 0.906)
15:  1300 ####### (0.013 / 0.923)
16:  1037 ##### (0.010 / 0.936)
17:   869 ##### (0.008 / 0.946)
18:   758 #### (0.007 / 0.955)
19:   644 #### (0.006 / 0.962)
20:   508 ### (0.005 / 0.969)
21:   418 ### (0.004 / 0.974)
22:   342 ## (0.003 / 0.978)
23:   271 ## (0.002 / 0.981)
24:   271 ## (0.002 / 0.984)
25:   226 ## (0.002 / 0.987)
26:   162 # (0.001 / 0.989)
27:   139 # (0.001 / 0.991)
28:   121 # (0.001 / 0.992)
29:   110 # (0.001 / 0.993)
30:    80 # (0.000 / 0.994)
31:    70 # (0.000 / 0.995)
32:    49 # (0.000 / 0.996)
33:    53 # (0.000 / 0.996)
34:    53 # (0.000 / 0.997)
35:    29 # (0.000 / 0.997)
36:    30 # (0.000 / 0.998)
37:    33 # (0.000 / 0.998)
38:    21 # (0.000 / 0.998)
39:    19 # (0.000 / 0.999)
40:    12 # (0.000 / 0.999)
41:    12 # (0.000 / 0.999)
42:    10 # (0.000 / 0.999)
43:    10 # (0.000 / 0.999)
44:     6 # (6e-5  / 0.999)
45:     6 # (6e-5  / 0.999)
46:     4 # (4e-5  / 0.999)
47:     2 # (2e-5  / 0.999)
48:     1 # (1e-5  / 0.999)
49:     2 # (2e-5  / 0.999)
50:     4 # (4e-5  / 0.999)
51:     1 # (1e-5  / 0.999)
52:     1 # (1e-5  / 0.999)
54:     4 # (4e-5  / 0.999)
55:     2 # (2e-5  / 0.999)
59:     1 # (1e-5  / 0.999)
67:     1 # (1e-5  / 0.999)
70:     1 # (1e-5  / 0.999)


  Cf gamble_dice_6_throws.rkt and gamble_dice_6_throws2.rkt

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

   (define p 1/6)

   ; Geometric distribution: number of failures until success
   (define d (+ 1 (geometric_dist_mean p)))

   ; Toss until a 6
   (define (toss n)
     (if (= (add1 (random-integer 6)) 6)
         n
         (f (add1 n))
     ))

   (define v (toss 1))
   
   (list d v)

   )
)

(show-marginals (model)
                (list  "d"
                       "v"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.9 0.95 0.99 0.999)
                #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


