#| 

  Hypothesis testing in Racket/Gamble 

  From Statistics101 (Resample Stats)
  http://www.statistics101.net/QuickReference.pdf
  Page 46
  """
  Here is another example of hypothesis testing, this one taken from 
  CliffsQuickReview Statistics p. 80, example 5: “A professor wants to 
  know if her introductory statistics class has a good grasp of basic 
  math. Six students are chosen at random from the class and given a 
  math proficiency test. The professor wants the class to be able to  
  score at least 70 on the test. The six students get scores of 
  62 92 75 68 83 95. 
  Can the professor be at least 90 percent certain that the mean score
  for the class on the test would be at least 70?”
  ->
  probability: 0.0304
  """

mean: 79.31666666666666
var : m
mean: 79.31666666666669
Credible interval (0.9): 71.95..87.16666666666667
HPD interval (0.9): 71.95..87.16666666666667
Histogram:
65.767:   2 ## (0.002 / 0    )
67.078:   2 ## (0.002 / 0.002)
68.39 :   2 ## (0.002 / 0.004)
69.702:   8 ####### (0.008 / 0.006)
71.013:  21 ################# (0.021 / 0.014)
72.325:  44 ################################### (0.044 / 0.035)
73.637:  51 ######################################## (0.051 / 0.079)
74.948:  71 ######################################################## (0.071 / 0.13 )
76.26 :  84 ################################################################## (0.084 / 0.201)
77.572: 100 ############################################################################## (0.1   / 0.285)
78.883:  94 ########################################################################## (0.094 / 0.385)
80.195:  91 ####################################################################### (0.091 / 0.479)
81.507: 103 ################################################################################ (0.103 / 0.57 )
82.818:  94 ########################################################################## (0.094 / 0.673)
84.13 :  71 ######################################################## (0.071 / 0.767)
85.442:  65 ################################################### (0.065 / 0.838)
86.753:  41 ################################ (0.041 / 0.903)
88.065:  22 ################## (0.022 / 0.944)
89.377:  18 ############## (0.018 / 0.966)
90.688:  14 ########### (0.014 / 0.984)

var : p
mean: 73/2592 (0.02816358024691358)
Credible interval (0.9): 0..0
Histogram:
#f: 978 ################################################################################ (0.978 / 0    )
#t:  22 ## (0.022 / 0.978)

r
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define scores '(62.9 92.0 75.0 68.0 83.0 95.0))
(show "mean" (avg scores))

(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define sample (resample (length scores) scores))
   (define m (avg sample))
   ;; What is the probability of a mean < 70?
   (define p (< m 70))
   
   (list m
         p
         )

   )
)

(show-marginals (model)
                (list  "m"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.90
                #:hpd-interval (list 0.90)
                #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


