#| 

  Heights in Racket/Gamble 

  From "Probabilistic programming", John Winn and Tom Minka, Machine Learning Summer School
  (ProbabilisticProgramming_slides.pdf)

  """
  Suppose we take a woman at random and a man at random from the UK population
  The woman turns out to be taller than the man.
  - What is the probability of this event?
  - What is the posterior distribution over the woman’s height?
  - What is the posterior distribution over the man’s height?

  ....

  double heightMan = random((Normal 177,64));
  double heightWoman = random((Normal 164,64));
  Bernoulli dist = infer(heightWoman > heightMan);
  constrain(heightWoman > heightMan);
  Gaussian distWoman = infer(heightWoman);
  Gaussian distMan = infer(heightMan);

  """

  I have split this into two questions:
  - model with obs #f: What is the probability?
  - model with obs #t: Posterior distributions (we observe that the woman is taller than the man)

  (And I just added the "graphical" output in the show-histogram function so it's showed here.)

* obs: #f
var : height_female
mean: 164.12365550397337
HPD interval (0.84): 153.35146938699654..176.1024928074725
HPD interval (0.95): 146.88803622219294..179.2622452524687
HPD interval (0.99): 143.79739043165424..185.27681490075392
Histogram:
137.467:   1 # (0.001 / 0    )
139.907:   4 ### (0.004 / 0.001)
142.347:   2 ## (0.002 / 0.005)
144.787:   4 ### (0.004 / 0.007)
147.227:  11 ######## (0.011 / 0.011)
149.667:  20 ############## (0.02  / 0.022)
152.107:  37 ######################### (0.037 / 0.042)
154.547:  49 ################################# (0.049 / 0.079)
156.987:  80 ###################################################### (0.08  / 0.128)
159.427: 100 #################################################################### (0.1   / 0.208)
161.867: 119 ################################################################################ (0.119 / 0.308)
164.307: 102 ##################################################################### (0.102 / 0.427)
166.747: 115 ############################################################################## (0.115 / 0.529)
169.187: 107 ######################################################################## (0.107 / 0.644)
171.627:  78 ##################################################### (0.078 / 0.751)
174.067:  68 ############################################## (0.068 / 0.829)
176.507:  41 ############################ (0.041 / 0.897)
178.947:  28 ################### (0.028 / 0.938)
181.387:  16 ########### (0.016 / 0.966)
183.827:  10 ####### (0.01  / 0.982)

var : height_male
mean: 177.02686998277983
HPD interval (0.84): 166.06601752314117..188.06310876694351
HPD interval (0.95): 160.28939822130144..192.71035141731423
HPD interval (0.99): 156.43595559322705..197.1624460341327
Histogram:
152.229:   1 # (0.001 / 0    )
154.965:   2 ## (0.002 / 0.001)
157.701:  10 ###### (0.01  / 0.003)
160.436:  12 ######## (0.012 / 0.013)
163.172:  19 ############ (0.019 / 0.025)
165.908:  27 ################# (0.027 / 0.044)
168.643:  68 ######################################### (0.068 / 0.071)
171.379:  85 ################################################### (0.085 / 0.139)
174.115: 121 ######################################################################### (0.121 / 0.224)
176.85 : 134 ################################################################################ (0.134 / 0.345)
179.586: 126 ############################################################################ (0.126 / 0.479)
182.322: 130 ############################################################################## (0.13  / 0.605)
185.057:  90 ###################################################### (0.09  / 0.735)
187.793:  82 ################################################# (0.082 / 0.825)
190.529:  40 ######################## (0.04  / 0.907)
193.264:  29 ################## (0.029 / 0.947)
196    :  16 ########## (0.016 / 0.976)
198.736:   5 ### (0.005 / 0.992)
201.471:   1 # (0.001 / 0.997)
204.207:   1 # (0.001 / 0.998)

var : diff
mean: -12.903214478806358
HPD interval (0.84): -30.04691764850932..1.2295346970921628
HPD interval (0.95): -35.52082820128231..8.766260926306188
HPD interval (0.99): -43.23179058174571..12.701492598388114
Histogram:
-44.706:   1 # (0.001 / 0    )
-40.876:   6 #### (0.006 / 0.001)
-37.046:  13 ######## (0.013 / 0.007)
-33.216:  22 ############# (0.022 / 0.02 )
-29.386:  41 ######################## (0.041 / 0.042)
-25.556:  58 ################################## (0.058 / 0.083)
-21.726:  89 #################################################### (0.089 / 0.141)
-17.896: 117 #################################################################### (0.117 / 0.23 )
-14.066: 138 ################################################################################ (0.138 / 0.347)
-10.236: 138 ################################################################################ (0.138 / 0.485)
-6.406 : 126 ########################################################################## (0.126 / 0.623)
-2.576 :  99 ########################################################## (0.099 / 0.749)
1.254  :  67 ####################################### (0.067 / 0.848)
5.084  :  39 ####################### (0.039 / 0.915)
8.914  :  25 ############### (0.025 / 0.954)
12.744 :  14 ######### (0.014 / 0.979)
16.574 :   1 # (0.001 / 0.993)
20.404 :   3 ## (0.003 / 0.994)
24.234 :   1 # (0.001 / 0.997)
28.064 :   1 # (0.001 / 0.998)

var : p
mean: 0.1490000000000001
Histogram:
#f: 900 ################################################################################ (0.9   / 0    )
#t: 100 ######### (0.1   / 0.9  )

* obs: #t
var : height_female
mean: 173.44316262065493
HPD interval (0.84): 164.64523011281537..181.16960950265403
HPD interval (0.95): 162.76464088133213..186.22869698359517
HPD interval (0.99): 157.18290352065443..188.97817915521665
Histogram:
154.504:   1 # (0.001 / 0    )
156.476:   1 # (0.001 / 0.001)
158.448:   4 ### (0.004 / 0.002)
160.42 :   7 ##### (0.007 / 0.006)
162.392:  15 ######### (0.015 / 0.013)
164.364:  28 ################# (0.028 / 0.028)
166.336:  52 ############################## (0.052 / 0.056)
168.308:  88 ################################################### (0.088 / 0.108)
170.28 : 100 ########################################################## (0.1   / 0.196)
172.252: 139 ################################################################################ (0.139 / 0.296)
174.225: 132 ############################################################################ (0.132 / 0.435)
176.197: 123 ####################################################################### (0.123 / 0.567)
178.169: 100 ########################################################## (0.1   / 0.69 )
180.141:  78 ############################################# (0.078 / 0.79 )
182.113:  52 ############################## (0.052 / 0.868)
184.085:  30 ################## (0.03  / 0.92 )
186.057:  27 ################ (0.027 / 0.95 )
188.029:  12 ####### (0.012 / 0.977)
190.001:   6 #### (0.006 / 0.989)
191.973:   3 ## (0.003 / 0.995)

var : height_male
mean: 167.58791405882954
HPD interval (0.84): 159.03269212961192..175.73381913872163
HPD interval (0.95): 155.56499216427252..179.01379889810514
HPD interval (0.99): 153.92690631242988..183.35634008412728
Histogram:
149.048:   1 # (0.001 / 0    )
150.901:   1 # (0.001 / 0.001)
152.755:   1 # (0.001 / 0.002)
154.609:   9 ###### (0.009 / 0.003)
156.462:  18 ########### (0.018 / 0.012)
158.316:  32 ################### (0.032 / 0.03 )
160.169:  44 ######################### (0.044 / 0.062)
162.023:  64 ##################################### (0.064 / 0.106)
163.877:  85 ################################################# (0.085 / 0.17 )
165.73 :  89 ################################################### (0.089 / 0.255)
167.584: 123 ###################################################################### (0.123 / 0.344)
169.438: 141 ################################################################################ (0.141 / 0.467)
171.291: 104 ############################################################ (0.104 / 0.608)
173.145: 101 ########################################################## (0.101 / 0.712)
174.999:  85 ################################################# (0.085 / 0.813)
176.852:  40 ####################### (0.04  / 0.898)
178.706:  26 ############### (0.026 / 0.938)
180.559:  16 ########## (0.016 / 0.964)
182.413:  13 ######## (0.013 / 0.98 )
184.267:   4 ### (0.004 / 0.993)

var : diff
mean: 5.855248561825002
HPD interval (0.84): 0.02184517406826103..10.298003467531885
HPD interval (0.95): 0.006937371640532319..14.652058853320625
HPD interval (0.99): 0.006937371640532319..20.144073083604553
Histogram:
0.007 :   1 # (0.001 / 0    )
1.306 : 176 ################################################################################ (0.176 / 0.001)
2.604 : 166 ############################################################################ (0.166 / 0.177)
3.903 : 126 ########################################################## (0.126 / 0.343)
5.201 :  98 ############################################# (0.098 / 0.469)
6.5   :  95 ############################################ (0.095 / 0.567)
7.799 :  70 ################################ (0.07  / 0.662)
9.097 :  58 ########################### (0.058 / 0.732)
10.396:  56 ########################## (0.056 / 0.79 )
11.694:  38 ################## (0.038 / 0.846)
12.993:  32 ############### (0.032 / 0.884)
14.292:  32 ############### (0.032 / 0.916)
15.59 :  12 ###### (0.012 / 0.948)
16.889:  11 ##### (0.011 / 0.96 )
18.188:  12 ###### (0.012 / 0.971)
19.486:   5 ### (0.005 / 0.983)
20.785:   3 ## (0.003 / 0.988)
22.083:   1 # (0.001 / 0.991)
23.382:   2 # (0.002 / 0.992)
24.681:   2 # (0.002 / 0.994)

var : p
mean: 1.0000000000000007
Histogram:
#t: 1000 ################################################################################ (1.0   / 0    )



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (model [obs #f])
  (show "* obs" obs)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define height_male (normal 177 (sqrt 64)))
   (define height_female (normal 164 (sqrt 64)))

   (define diff (- height_female height_male))
   (define p (> height_female height_male))

   (when obs
     (observe/fail (> height_female height_male)))
   
   (list height_female
         height_male
         diff
         p
    )
   )
)

(for ([obs '(#f #t)])
  (show-marginals (model obs)
                  (list  "height_female"
                         "height_male"
                         "diff"
                         "p"
                         )
                  #:num-samples 1000
                  #:truncate-output 5
                  #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.94
                  #:hpd-interval (list 0.84 0.95 0.99)
                  #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )
  )

