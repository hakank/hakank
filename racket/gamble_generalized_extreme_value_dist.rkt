#| 

  Generalized extreme value dist in Racket/Gamble 

  This is from Wikipedia 
  https://en.wikipedia.org/wiki/Generalized_extreme_value_distribution
  GEV(mu, sigma, xi)

  Note: Cf min_stable_dist and max_stable_dist.
  Also see gamble_generalized_extreme_value_dist.rkt which uses a 
  different form.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


; Comparing with R and Matlab
(displayln "PDF")
(generalized_extreme_value_pdf 2 2 0 1) ;  0.158521 ok
(generalized_extreme_value_pdf 2 2 0 -1) ;  0.02535356 ok
(generalized_extreme_value_pdf 2 2 1 1) ;  0.2706706 ok
(generalized_extreme_value_pdf 2 2 1 -1) ; 0 ok
(generalized_extreme_value_pdf 2 2 -1 1) ; 0.1115651 ok
(generalized_extreme_value_pdf 2 2 -1 -1) ; 0.0410425 ok

(generalized_extreme_value_pdf 100 15 1 100) ; 
(newline)
(displayln "CDF")
(generalized_extreme_value_cdf 2 2 0 1)  ; 0.1922956 ok
(generalized_extreme_value_cdf 2 2 0 -1) ;  0.01131429 ok
(generalized_extreme_value_cdf 2 2 1 1) ; 0.1353353 ok
(generalized_extreme_value_cdf 2 2 1 -1) ; 0 ok
(generalized_extreme_value_cdf 2 2 -1 1) ;  0.2231302 ok
(generalized_extreme_value_cdf 2 2 -1 -1) ; 0.082085 ok
(generalized_extreme_value_cdf 10 5 10 100) ; 
(newline)
(displayln "Quantile")
(generalized_extreme_value_quantile 2 2 0 0.01) ; -1.054359 ok
(generalized_extreme_value_quantile 2 2 0 0.99) ; 11.2003  ok
(generalized_extreme_value_quantile 2 2 0 0.9999)  
(generalized_extreme_value_quantile 2 2 1 0.01) ; 0.4342945 ok 
(generalized_extreme_value_quantile 2 2 1 0.99) ; 198.9983 ol
(generalized_extreme_value_quantile 2 2 1 0.9999) ; 198.9983 ok
(generalized_extreme_value_quantile 2 2 -1 0.01) ; -5.21034 ok
(generalized_extreme_value_quantile 2 2 -1 0.99) ; 3.979899 ok
(generalized_extreme_value_quantile 2 2 -1 0.9999) 
(newline)
(displayln "Mean")
(generalized_extreme_value_mean 2 2 0) ; 3.15483
(avg (for/list ([i 100000]) (generalized_extreme_value_dist 2 2 0)))

(generalized_extreme_value_mean 2 2 0.99) ; 28.30227
(avg (for/list ([i 100000]) (generalized_extreme_value_dist 2 2 0.99)))

(generalized_extreme_value_mean 2 2 1) ; 33.65967
(avg (for/list ([i 100000]) (generalized_extreme_value_dist 2 2 1)))

(generalized_extreme_value_mean 2 2 -1) ; 2.000483
(avg (for/list ([i 100000]) (generalized_extreme_value_dist 2 2 -1)))


(newline)
(displayln "Dist")
(for/list ([i 4]) (generalized_extreme_value_dist 2 2 0))
(for/list ([i 4]) (generalized_extreme_value_dist 2 2 1))
(for/list ([i 4]) (generalized_extreme_value_dist 2 2 -1))
(for/list ([i 4]) (generalized_extreme_value_dist 2 2 10))

(newline)

#|

  Histogram for GEV 10 3 0.1

Histogram for (generalized_extreme_value_dist 10 3 0.1)
3.464 :    1 # (0.000 / 0    )
4.608 :    7 # (0.000 / 0.000)
5.752 :  103 ###### (0.010 / 0.000)
6.897 :  425 ######################### (0.042 / 0.011)
8.041 :  946 ###################################################### (0.094 / 0.053)
9.185 : 1228 ###################################################################### (0.122 / 0.148)
10.329: 1412 ################################################################################ (0.141 / 0.271)
11.474: 1312 ########################################################################### (0.131 / 0.412)
12.618: 1088 ############################################################## (0.108 / 0.543)
13.762:  855 ################################################# (0.085 / 0.652)
14.906:  676 ####################################### (0.067 / 0.737)
16.051:  493 ############################ (0.049 / 0.805)
17.195:  347 #################### (0.034 / 0.854)
18.339:  271 ################ (0.027 / 0.889)
19.483:  196 ############ (0.019 / 0.916)
20.628:  148 ######### (0.014 / 0.936)
21.772:  111 ####### (0.011 / 0.950)
22.916:   85 ##### (0.008 / 0.961)
24.061:   59 #### (0.005 / 0.970)
25.205:   46 ### (0.004 / 0.976)
26.349:   35 ## (0.003 / 0.980)
27.493:   31 ## (0.003 / 0.984)
28.638:   20 ## (0.002 / 0.987)
29.782:   28 ## (0.002 / 0.989)
30.926:   15 # (0.001 / 0.992)
32.07 :    7 # (0.000 / 0.993)
33.215:   10 # (0.001 / 0.994)
34.359:    8 # (0.000 / 0.995)
35.503:    7 # (0.000 / 0.996)
36.648:    5 # (0.000 / 0.997)
37.792:    2 # (0.000 / 0.997)
38.936:    5 # (0.000 / 0.997)
40.08 :    2 # (0.000 / 0.998)
41.225:    2 # (0.000 / 0.998)
42.369:    2 # (0.000 / 0.998)
43.513:    2 # (0.000 / 0.998)
44.657:    1 # (0.000 / 0.999)
45.802:    2 # (0.000 / 0.999)
46.946:    1 # (0.000 / 0.999)
48.09 :    3 # (0.000 / 0.999)
49.235:    1 # (0.000 / 0.999)
50.379:    1 # (0.000 / 0.999)
51.523:    0  (0     / 0.999)
52.667:    0  (0     / 0.999)

|#
(displayln "Histogram for (generalized_extreme_value_dist 10 3 0.1)")
(show-histogram (for/list ([i 10000]) (generalized_extreme_value_dist 10 3 0.1)))

(newline)
#|

  variable : d
  9.834951431627918: 0.00010000000000000938
  9.655433719747188: 0.00010000000000000938
  15.641779712905933: 0.00010000000000000938
  7.520270760843153: 0.00010000000000000938
  14.235902617692062: 0.00010000000000000938
  ...
  12.884353234381226: 0.00010000000000000938
  11.29877710300728: 0.00010000000000000938
  12.256941666701254: 0.00010000000000000938
  10.322005332326965: 0.00010000000000000938
  10.035817567375648: 0.00010000000000000938
  mean: 12.069836290130644
  HPD interval (0.84): 5.98793583629543..16.173729698843008
  HPD interval (0.9): 5.896825871948551..18.1534880987395
  HPD interval (0.99): 4.535447059114677..27.754067916015515
  HPD interval (0.999): 4.509374891532706..40.42165287578726
  HPD interval (0.9999): 3.6769205525501185..48.02575773248818
  HPD interval (0.99999): 3.6769205525501185..48.02575773248818

|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define mu 10)
   (define sigma 3)   
   (define xi 0.1)   

   (define d (generalized_extreme_value_dist mu sigma xi))

   (list d
         )
   )
)

(show-marginals (model)
              (list  "d"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    #:hpd-interval (list 0.84 0.9 0.99 0.999 0.9999 0.99999)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


(newline)

#|
  Recover parameters. 

  Mathematica's FindDistributionParameter gives 
  {mu -> 146.64, sigma -> 4.56746, xi -> 0.0144781}

  This model:

  variable : mu
  145.91182066983916: 0.028927045953377244
  148.9091748558853: 0.022084482683369412
  148.38429267687664: 0.016123171326154043
  148.76064648928042: 0.015906975748908727
  148.78628658224454: 0.015099195140068666
  ...
  150.75364717922812: 1.0375614698512104e-180
  145.3438681692397: 1.0375614698512104e-180
  147.5774876652802: 1.0375614698512104e-180
  145.96800264764883: 1.0375614698512104e-180
  145.47418911575252: 1.0375614698512104e-180
  mean: 148.02857435351925
  HPD interval (0.01): 145.4157548631963..145.80055865051384
  HPD interval (0.84): 146.86751190780112..149.5940094567115
  HPD interval (0.99): 145.51997596949494..150.62937252436495
  HPD interval (0.9999): 144.66837772733726..151.37849349348107

  variable : sigma
  0.3340980386110936: 0.028927045953377244
  0.1186141242905841: 0.022084482683369412
  0.4953812395779644: 0.016123171326154043
  0.14622096214768482: 0.015906975748908727
  0.09063984240709974: 0.015099195140068666
  ...
  2.061282002540924: 1.0375614698512104e-180
  18.859078740395695: 1.0375614698512104e-180
  18.101354871197096: 1.0375614698512104e-180
  15.639128380673636: 1.0375614698512104e-180
  19.923590972113175: 1.0375614698512104e-180
  mean: 0.43586713966918716
  HPD interval (0.01): 0.0007589999953918158..0.0007589999953918158
  HPD interval (0.84): 0.0005504302947059044..0.8918458329289997
  HPD interval (0.99): 0.0007589999953918158..2.290299818008757
  HPD interval (0.9999): 0.0005504302947059044..2.8246543620545674
 
  variable : xi
  0.660023453478906: 0.028927045953377244
  0.717068597010865: 0.022084482683369412
  0.608927337372882: 0.016123171326154043
  0.43956747288583653: 0.015906975748908727
  0.2383007559847453: 0.015099195140068666
  ...
  0.5252266126328929: 1.0375614698512104e-180
  0.9167001912541781: 1.0375614698512104e-180
  0.6371956801825905: 1.0375614698512104e-180
  0.039327188902547425: 1.0375614698512104e-180
  0.0700521675336293: 1.0375614698512104e-180
  mean: 0.3566495925649728
  HPD interval (0.01): 0.0058782166388512275..0.007072825094486499
  HPD interval (0.84): 0.018264532275270372..0.616648867321891
  HPD interval (0.99): 0.0058782166388512275..0.9467736729255236
  HPD interval (0.9999): 0.0058782166388512275..0.9828928945217567

  variable : post
  145.87791608291624: 0.028927045953377244
  149.2104286547027: 0.022084482683369412
  148.9465935979218: 0.016123171326154043
  149.13855023225733: 0.015906975748908727
  148.71436781442887: 0.015099195140068666
  ...
  159.5269707515429: 1.0375614698512104e-180
  151.75075697973338: 1.0375614698512104e-180
  165.55778177700398: 1.0375614698512104e-180
  139.51930511020862: 1.0375614698512104e-180
  188.70597561311277: 1.0375614698512104e-180
  mean: 148.49148161053733
  HPD interval (0.01): 145.37712586659353..145.77564599628414
  HPD interval (0.84): 146.86476191209456..149.72754721062782
  HPD interval (0.99): 145.03159818926875..153.0907043517149
  HPD interval (0.9999): 144.4931666404606..169.30115910166415

  variable : p
  #f: 0.9995951657115244
  #t: 0.00040483428847567636
  mean: 0.00040483428847567636

  mh-sampler is not bad (either), but it's slower.

|#

; Generate 100 max points of 1000 samples
(define data (for/list ([i 100])
               (max-list (for/list ([i 1000])
               ; (min-list (for/list ([i 1000]) 
                           (normal 100 15))
                           ; (exponential 10))
                           ; (generalized_extreme_value_dist 10 3 0.1))
                         )))
(show "data" data)
(show2 "min" (min-list data) "mean" (avg data) "max" (max-list data) "stddev" (stddev data) "variance" (variance data))
(newline)
(show-histogram data)
(flush-output)
(newline)

(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define mu (normal (avg data) (stddev data)))
   (define sigma (uniform 0 20))
   ; (define xi (uniform -3 3))
   (define xi (uniform 0 1)) ; we know it's maximal values

   ; For importance-sampler this should be about 2
   ; For mh-sampler it can be quite smaller, e.g. 0.1, but it's slower.
   (define observe_sigma 2)

   (for ([i (range (length data))])
     (observe-sample (normal-dist (generalized_extreme_value_dist mu sigma xi) observe-sigma)
                     (list-ref data i))
     )
   
   (define post (generalized_extreme_value_dist mu sigma xi))
   (define p (>= post (max-list data)))

   ; (show2 "mu" mu "sigma" sigma "xi" xi "post" post "p" p)
   
   (list mu
         sigma
         xi
         post
         p
         )
   )
)

(show-marginals (model2)
                (list  "mu"
                       "sigma"
                       "xi"
                       "post"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.01 0.84 0.99 0.9999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 1000
                ; #:thin 10
                )
