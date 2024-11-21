#| 

  Coupon Collor problem in Racket/Gamble 

  From Siegrist "Probability Mathematical Statisics and Stochastic Processes"
  """
  Suppose that a standard, fair die is thrown until all 6 scores have occurred. 
  Find each of the following:
  1. The probability density function of the number of throws.
  2. The mean of the number of throws.
  3. The variance of the number of throws.
  4. The probability that at least 10 throws are required.
  """
  
  Mean
  14.7
  Variance
  38.99
  w >= 10
  0.8109567901234568

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; 2. The mean of the number of throws.
(displayln "Mean")
(* 1.0 (coupon_collector_mean 6 6))
;  3. The variance of the number of throws.

(displayln "Variance")
(* 1.0 (coupon_collector_variance 6 6))

;  4. The probability that at least 10 throws are required.
(displayln "w >= 10")
(- 1.0 (coupon_collector_cdf 6 6 9))


#|
Modeling the dice problem
variable : d
9: 0.08999999999999998
11: 0.08999999999999998
12: 0.08299999999999999
13: 0.07899999999999999
10: 0.07299999999999998
15: 0.06399999999999999
14: 0.060999999999999985
16: 0.051999999999999984
8: 0.04599999999999999
17: 0.04399999999999999
7: 0.03799999999999999
19: 0.03699999999999999
20: 0.03599999999999999
18: 0.033999999999999996
22: 0.023999999999999994
21: 0.021999999999999995
25: 0.021999999999999995
24: 0.019999999999999997
23: 0.015999999999999997
28: 0.014999999999999996
26: 0.010999999999999998
6: 0.008999999999999998
27: 0.005999999999999998
30: 0.004999999999999999
32: 0.004999999999999999
29: 0.003999999999999999
33: 0.002999999999999999
31: 0.0019999999999999996
34: 0.0019999999999999996
35: 0.0019999999999999996
36: 0.0019999999999999996
40: 0.0009999999999999998
41: 0.0009999999999999998
45: 0.0009999999999999998
mean: 14.848999999999998
Min: 6 Mean: 14.812 Max: 56 Variance: 39.924656 Stddev: 6.318596046591363

variable : p
#t: 0.8170000000000002
#f: 0.18299999999999994
mean: 0.8170000000000002
Min: 0 Mean: 0.812 Max: 1 Variance: 0.152656 Stddev: 0.3907121702737195


|#
(displayln "\nModeling the dice problem")
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define m 6)
   (define d (coupon_collector_dist m m))
   ; (define d (coupon_collector_dist2 m m 1000))   
   (define p (>= d 10))

   (list d
         p)
   )
)

(show-marginals (model)
                (list  "d"
                       "p"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

#|
  A more pure simulation of coupon collector problem. For m=k=10.
  The problem:
  """
  A box of a certain brand of cereal comes with a special toy. There are 
  10 different toys in all. A collector buys boxes of cereal
  until she has all 10 toys. Find each of the following:
  1. The probability density function of the number boxes purchased.
  2. The mean of the number of boxes purchased.
  3. The variance of the number of boxes purchased.
  4. The probability that no more than 15 boxes were purchased.
  """

Mean
29.28968253968254
Variance
125.68709057697153
b <= 15
143594451/3125000000
0.04595022432

variable : len
23: 0.04750000000000003
24: 0.04680000000000003
21: 0.044700000000000024
22: 0.04410000000000003
27: 0.04150000000000002
20: 0.04070000000000002
25: 0.03940000000000002
28: 0.03940000000000002
19: 0.03930000000000002
26: 0.03930000000000002
18: 0.03560000000000002
30: 0.03370000000000002
29: 0.03170000000000002
17: 0.031200000000000016
32: 0.030400000000000017
31: 0.029900000000000017
34: 0.029500000000000016
33: 0.026900000000000014
35: 0.023900000000000015
16: 0.023000000000000013
36: 0.02080000000000001
15: 0.02020000000000001
38: 0.01860000000000001
39: 0.01840000000000001
37: 0.01680000000000001
40: 0.01630000000000001
14: 0.013800000000000007
42: 0.013300000000000006
41: 0.012600000000000007
44: 0.011000000000000006
43: 0.010700000000000005
13: 0.009400000000000006
45: 0.009400000000000006
46: 0.009300000000000004
47: 0.007300000000000004
48: 0.007000000000000004
50: 0.005200000000000002
52: 0.004800000000000002
53: 0.004800000000000002
49: 0.004700000000000003
12: 0.003900000000000002
51: 0.003800000000000002
55: 0.003800000000000002
54: 0.003500000000000002
56: 0.0031000000000000016
58: 0.002600000000000001
60: 0.0025000000000000014
59: 0.002400000000000001
63: 0.001800000000000001
62: 0.0017000000000000008
61: 0.001600000000000001
57: 0.001500000000000001
68: 0.001500000000000001
64: 0.0013000000000000006
65: 0.0013000000000000006
67: 0.0012000000000000005
11: 0.0011000000000000007
66: 0.0011000000000000007
69: 0.0010000000000000007
73: 0.0007000000000000004
72: 0.0006000000000000003
10: 0.00040000000000000024
70: 0.00040000000000000024
71: 0.00040000000000000024
74: 0.00040000000000000024
83: 0.00040000000000000024
76: 0.00030000000000000014
80: 0.00030000000000000014
77: 0.00020000000000000012
78: 0.00020000000000000012
81: 0.00020000000000000012
82: 0.00020000000000000012
86: 0.00020000000000000012
87: 0.00020000000000000012
88: 0.00020000000000000012
79: 0.00010000000000000006
84: 0.00010000000000000006
89: 0.00010000000000000006
90: 0.00010000000000000006
91: 0.00010000000000000006
93: 0.00010000000000000006
94: 0.00010000000000000006
96: 0.00010000000000000006
98: 0.00010000000000000006
101: 0.00010000000000000006
114: 0.00010000000000000006
mean: 29.264200000000017
Min: 10 Mean: 29.0678 Max: 103 Variance: 120.94880316 Stddev: 10.997672624696554

variable : p
#f: 0.9543999999999999
#t: 0.04560000000000003
mean: 0.04560000000000003
Min: 0 Mean: 0.046 Max: 1 Variance: 0.043884 Stddev: 0.2094850830011531

|#
(displayln "\nModel 2")
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define m 10)
   (define (f a)
     (if (and (>= (length a) m) (= m (length (remove-duplicates a))))
         a
         (f (append a (list (random-integer m))))))

   (define a (f '()))
   (define len (length a))

   (define p (<= len 15))

   (list len
         p)
   )
)

(show-marginals (model2)
                (list  "len"
                       "p"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


; The mean of the number of boxes purchased.
(displayln "Mean")
(* 1.0 (coupon_collector_mean 10 10))

; The variance of the number of boxes purchased.
(displayln "Variance")
(* 1.0 (coupon_collector_variance 10 10))

;  4. The probability that no more than 15 boxes were purchased
(displayln "b <= 15")
(coupon_collector_cdf 10 10 15)
(* 1.0 (coupon_collector_cdf 10 10 15))
