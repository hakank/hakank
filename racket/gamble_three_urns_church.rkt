#| 

  Three urns in Racket/Gamble 

  From Andreas Stuhlmüller 
  "Modeling Cognition with Probabilistic Programs: Representations and Algorithms"
  page 30ff
  """
  We are presented with three opaque urns, each of which contains some unknown number of
  red and black balls. We do not know the proportion of red balls in each urn, and
  we don’t know how similar the proportions of red balls are between urns, but we
  have reason to suspect that the urns could be similar, as they all were filled at the
  same factory. We are asked to predict the proportion of red balls in the third urn
  (1) before making any observations, 
  (2) after observing 15 balls drawn from the first urn, 14 of which are red, and 
  (3) after observing in addition 15 balls drawn from the second urn, only one 
      of which is red.
  """
  
  This is a port of the Church code at page 35 for scenario 2 and 3: we observe 
  14 red balls and 1 black.
  """
  (query
  ; ; model
  (define bias (uniform 0 10) )
  (define red-bias (uniform 0 bias ) )
  (define black-bias (- bias red-bias ) )
  (define urn->proportion-red
    (mem
      (λ (urn)
      (beta (+ .4 red-bias ) (+ .4 black-bias ) ) ) ) )
  (define (sample-urn urn )
     (if (flip (urn-> proportion-red urn ) ) R B) )

  ;; query expression
  (urn->proportion-red 3)

  ;; condition
  (equal? (repeat 15 (λ () (sample-urn 1) ) )
  (list R R R R R R B R R R R R R R R) ) )
  """

  This uses the Gamble utils for Church models, 
  i.e. the rejection-query function.

  Note the order of query expression and condition (with #:when).

  Output:

Min: 4.544108523319864e-8 Mean: 0.7264739430950539 Max: 0.9999898114384532 Variance: 0.05991947997373575 Stddev: 0.24478455828286177

HPD-interval:
HPD interval (0.84): 0.47641520208512655..0.9999898114384532
HPD interval (0.9): 0.34600137585113244..0.9999898114384532
HPD interval (0.95): 0.20778809667055612..0.9999898114384532
HPD interval (0.99): 0.03971849522579417..0.9999898114384532

Percentiles:
(0.01 0.03971849522579417)
(0.025 0.09861682485074397)
(0.1 0.34600137585113244)
(0.05 0.20778809667055612)
(0.25 0.5912432436147712)
(0.5 0.7937649152421901)
(0.75 0.9240462847986287)
(0.84 0.9584387873298565)
(0.9 0.98057695680957)
(0.95 0.991350971598128)
(0.975 0.9972223611153671)
(0.99 0.9992594911391497)
(0.999 0.9999845428813402)

Histogram:
0   :   1 # (0.001 / 0    )
0.05:  11 ##### (0.011 / 0.001)
0.1 :  14 ####### (0.014 / 0.012)
0.15:  12 ###### (0.012 / 0.026)
0.2 :   9 #### (0.009 / 0.038)
0.25:  22 ########## (0.022 / 0.047)
0.3 :  15 ####### (0.015 / 0.069)
0.35:  18 ######## (0.018 / 0.084)
0.4 :  19 ######### (0.019 / 0.102)
0.45:  23 ########## (0.023 / 0.121)
0.5 :  35 ################ (0.035 / 0.144)
0.55:  36 ################ (0.036 / 0.179)
0.6 :  41 ################## (0.041 / 0.215)
0.65:  46 #################### (0.046 / 0.256)
0.7 :  66 ############################# (0.066 / 0.302)
0.75:  61 ########################### (0.061 / 0.368)
0.8 :  80 ################################### (0.08  / 0.429)
0.85:  88 ####################################### (0.088 / 0.509)
0.9 :  94 ######################################### (0.094 / 0.597)
0.95: 125 ####################################################### (0.125 / 0.691)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (rejection-query
   
  ; model
  (define bias (uniform 0 10) )
  (define red-bias (uniform 0 bias ) )
  (define black-bias (- bias red-bias ) )
  (define urn->proportion-red
    (mem
      (λ (urn)
      (beta (+ .4 red-bias ) (+ .4 black-bias ) ) ) ) )
  (define (sample-urn urn )
     (if (flip (urn->proportion-red urn ) ) "R" "B") )

  ;; query expression
  (urn->proportion-red 3)
  
  ;; condition
  #:when
  (equal? (repeat (λ () (sample-urn 1) ) 15)
          (list "R" "R" "R" "R" "R" "R" "B" "R" "R" "R" "R" "R" "R" "R" "R")))

  
)

(define sample (repeat (lambda () (model)) 1000))
; (show-freq (repeat (lambda () (model)) 1000))

; (avg (repeat (lambda () (model)) 1000))
; (display sample)
(show-stats sample)
(displayln "\nHPD-interval:")
(show-hpd-interval sample '(0.84 0.90 0.95 0.99))
(displayln "\nPercentiles:")
(show-percentiles sample)
(displayln "\nHistogram:")
(show-histogram sample)
