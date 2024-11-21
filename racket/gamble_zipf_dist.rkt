#| 

  Zipf dist in Racket/Gamble 


  https://en.wikipedia.org/wiki/Zipf%27s_law and Mathematica's ZipfDistribution

  zipf_dist(n,s) can be used with Enumerate.

  Note that zipf1_dist(s) is approximated by zipf_dist(1000,s) so it's not especially reliable.
  If you want a more reliable approximation, use zipf_dist(<a larger value of n>, s).

 
  From Mathematica:
  """
  Rank 15 web pages according to popularity. The access frequencies follow Zipf distribution 
  with 0.3. Find the distribution of access frequencies:
  """
  d = ZipfDistribution(15, 0.3);
  Mean(d)
  -> 3.57738
  """

  variable : z
  1: 0.40531035255802167
  2: 0.16460715256662187
  3: 0.09716952184112392
  4: 0.06685127706480753
  5: 0.05001804248785702
  6: 0.03946308848412122
  7: 0.03229685582099085
  8: 0.027150054997682277
  9: 0.023295521358490362
  10: 0.020313637435904922
  11: 0.01794639432721853
  12: 0.016026994094422806
  13: 0.01444313061499415
  14: 0.013116599267685825
  15: 0.011991377080057002
  mean: 3.577379582807595
  HPD interval (0.5): 1..2
  HPD interval (0.9): 1..8
  HPD interval (0.95): 1..11
  HPD interval (0.99): 1..15
  HPD interval (0.999): 1..15

  zipf_mean 15 3/10: 3.5773795828075943


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")
(require (rename-in math/special-functions
                    (zeta zetaf)))


(show "zipf_mean 15 3/10" (zipf_mean 15 3/10))
(show "zipf1_mean 2" (zipf1_mean 2))
(newline)

#|
  (zipf n alpha) with enumerate

  Model 1 zipf n alpha with enumerate
  variable : z
  1: 0.40531035255802167
  2: 0.16460715256662187
  3: 0.09716952184112392
  4: 0.06685127706480753
  5: 0.05001804248785702
  6: 0.03946308848412122
  7: 0.03229685582099085
  8: 0.027150054997682277
  9: 0.023295521358490362
  10: 0.020313637435904922
  11: 0.01794639432721853
  12: 0.016026994094422806
  13: 0.01444313061499415
  14: 0.013116599267685825
  15: 0.011991377080057002
  mean: 3.577379582807595
  HPD interval (0.5): 1..2
  HPD interval (0.9): 1..9
  HPD interval (0.95): 1..12
  HPD interval (0.99): 1..15
  HPD interval (0.999): 1..15
  HPD interval (0.99999): 1..15
  Histogram:
 1: 40632 ################################################################################ (0.406 / 0    )
 2: 16453 ################################# (0.164 / 0.406)
 3:  9556 ################### (0.095 / 0.570)
 4:  6751 ############## (0.067 / 0.666)
 5:  5012 ########## (0.050 / 0.733)
 6:  3873 ######## (0.038 / 0.784)
 7:  3308 ####### (0.033 / 0.822)
 8:  2707 ###### (0.027 / 0.855)
 9:  2348 ##### (0.023 / 0.882)
10:  1976 #### (0.019 / 0.906)
11:  1828 #### (0.018 / 0.926)
12:  1608 #### (0.016 / 0.944)
13:  1479 ### (0.014 / 0.960)
14:  1296 ### (0.012 / 0.975)
15:  1173 ### (0.011 / 0.988)

  variable : p
  #f: 0.988008622919943
  #t: 0.011991377080057002
  mean: 0.011991377080057002
  Histogram:
#f: 98827 ################################################################################ (0.988 / 0    )
#t:  1173 # (0.011 / 0.988)

|#
(define (model1)
  
   (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

    (define n 15)
    (define alpha 3/10)
    
    (define z (zipf n alpha))
    (define p (>= z (zipf_quantile n alpha 0.99)))
   
    (list z
          p
          )
    )
  )

(displayln "Model 1 zipf n alpha with enumerate")
(show-marginals (model1)
                (list  "z"
                       "p"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.5 0.90 0.95 0.99 0.999 0.99999)
                #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
  zipf1 alpha with importance-sampler.

  Note: This is not very reliable for small value of alpha (near 1)

  variable : z
  1: 0.92428
  2: 0.0573
  3: 0.01144
  4: 0.00357
  5: 0.00168
  6: 0.00068
  7: 0.00028
  8: 0.00026
  9: 0.00016
  13: 9e-5
  10: 8e-5
  11: 4e-5
  12: 4e-5
  17: 2e-5
  22: 2e-5
  23: 2e-5
  39: 1e-5
  20: 1e-5
  24: 1e-5
  26: 1e-5
  mean: 1.1106599999999998
  HPD interval (0.5): 1..1
  HPD interval (0.9): 1..1
  HPD interval (0.95): 1..2
  HPD interval (0.99): 1..3
  HPD interval (0.999): 1..7
  HPD interval (0.99999): 1..23
  Histogram:
 1: 92365 ################################################################################ (0.923 / 0    )
 2:  5808 ###### (0.058 / 0.923)
 3:  1158 ## (0.011 / 0.981)
 4:   343 # (0.003 / 0.993)
 5:   146 # (0.001 / 0.996)
 6:    77 # (0.000 / 0.998)
 7:    35 # (0.000 / 0.998)
 8:    22 # (0.000 / 0.999)
 9:    10 # (0.000 / 0.999)
10:    12 # (0.000 / 0.999)
11:     7 # (7e-5  / 0.999)
12:     3 # (3e-5  / 0.999)
13:     4 # (4e-5  / 0.999)
14:     1 # (1e-5  / 0.999)
16:     2 # (2e-5  / 0.999)
17:     2 # (2e-5  / 0.999)
18:     1 # (1e-5  / 0.999)
20:     1 # (1e-5  / 0.999)
23:     3 # (3e-5  / 0.999)

variable : p
#f: 0.98158
#t: 0.018419999999999992
mean: 0.018419999999999992
Histogram:
#f: 98173 ################################################################################ (0.981 / 0    )
#t:  1827 ## (0.018 / 0.981)

|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define alpha 3)
   (define z (zipf1 alpha))

   (define p (>= z (zipf1_quantile alpha 0.99)))

   (list z
         p
         )
   )
)

(displayln "zipf1 alpha with importance-sampler")
(show-marginals (model2)
                (list  "z"
                       "p"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.5 0.90 0.95 0.99 0.999 0.99999)
                #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )
