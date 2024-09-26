#| 

  Baseball payroll in Racket.Gamble 

  From "Resampling Stats Illustrations" (http://www.statistics101.net/PeterBruce_05-illus.pdf)
  Page 45
  """
  Baseball payroll—hypothesis test for correlation using the Pearson
  correlation coefficient
  (program “baseball”)
  Is a baseball team’s performance directly related to its payroll? (In
  technical terms, is there a correlation between two variables, or are
  they independent?) Specifically, we want to know whether base-
  ball teams with high payrolls also tend to be the better performing
  teams.
  The following data are from the Washington Post, March 27, 1998,
  page F2, and were compiled by the Post according to the formula
  of the Player Relations Council. Performance is ranked by the
  teams’ won-loss records; note that good performance is denoted by
  a low rank number.

  TABLE 3. MAJOR LEAGUE PAYROLL AND WON-LOSS RANKS 1995–1997
  Total          Payroll    Rank*
  NY Yankees     192.7        3
  Baltimore      179.5        4
  Atlanta        164.8        1
  Cleveland      155.7        2
  Chicago WS     150.3       14
  Cincinnati     143 9.       5
  Texas          139.9       11
  Colorado       138.3        8
  Toronto        137.4       25
  St. Louis      137.3       19.5
  Seattle        137.1        6
  Boston         131.8        7
  Los Angeles    128.3        5
  San Francisco  124         18
  Chicago Cubs   123         21
  Florida        122.8       12
  Anaheim        116         15.5
  Houston        115.4        9.5
  Philadelphia   109.9       26
  San Diego      104.5       13
  NY Mets        104.2       17
  Kansas City    101.1       22
  Minnesota       94.6       27
  Oakland         85.5       23.5
  Detroit         84         28
  Milwaukee       78.5       19.5
  Pittsburgh      67.7       23.5
  Montreal        67.6       15.5
  * Rank in games won and lost over the 3-year period

  ...
  The Pearson correlation coefficient for these data is -.71. Is that statistically 
  significant? Might a correlation coefficient this negative have occurred
  just by chance? In other words, might there be no relationship
  between the two variables and the apparent correlation be the
  result of the “luck of the draw?”

  ...
  No shuffling produced a correlation coefficient as negative as the
  observed value, -.71, so we conclude that there is significant
  correlation between payroll and performance. The higher the
  payroll, the lower the rank number (i.e. the better the perfor-
  mance).
  """


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define payroll '(192.7 179.5 164.8 155.7 150.3 143 139.9 138.3 137.4 137.3 137.1 131.8 128.3 124 
                        123 122.8 116 115.4 109.9 104.5 104.2 101.1 94.6 85.5 84 78.5 67.7 67.6))
(define rank '(3 4 1 2 14 9.5 11 8 25 19.5 6 7 5 18 21 12 15.5 9.5 26 
                 13 17 22 27 23.5 28 19.5 23.5 15.5 ))


(define cc (correlation-coefficient payroll rank))
(show "correlation coefficient of original data:" cc)

#|

  Comparing resampled version using correlation coefficient

  * Model 1

  correlation coefficient of original data:: -0.7108560103566262
  var : r2
  0.2888555247598092: 0.0009999999999999994
  0.154992142939491: 0.0009999999999999994
  -0.4385672734719675: 0.0009999999999999994
  -0.23993105579633833: 0.0009999999999999994
  0.019426103388728105: 0.0009999999999999994
  ...
  0.27513368397857146: 0.0009999999999999994
  -0.3738662797149891: 0.0009999999999999994
  0.036366470175198756: 0.0009999999999999994
  -0.23886060853120344: 0.0009999999999999994
  -0.1106799366888772: 0.0009999999999999994
  mean: -0.004174579097193832
  Credible interval (0.95): -0.3628457018310012..0.37715664781855224

  var : p
  #f: 1.0000000000000007
  mean: 0 (0.0)
  Credible interval (0.95): 0..0


|#
(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

    (define payroll_sample (draw-without-replacement (length payroll) payroll))
    (define rank_sample    (draw-without-replacement (length rank) rank))
    (define r2 (correlation-coefficient payroll_sample rank_sample))
    (define p (<= r2 cc)) ;; Probability that the sample coefficient is as low as the original
    
    (list r2
          p
          )
    
   )
)

(displayln "Model 1")
(show-marginals (model1)
                (list  "r2"
                       "p"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.95
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


#|

  From "Resampling Stats Illustrations" (http://www.statistics101.net/PeterBruce_05-illus.pdf)
  Page 45
  """
  Baseball payroll—testing for correlation using the sum-of-products statistic
  (program “basebal2”)
  We can also conduct this test with a statistic other than the correla-
  tion coefficient, the calculation of which is not wholly transparent.
  We will use the “sum of products” statistic.
  ...
  -> 
  prob = .0001
  """

  sum_products_orig: 44858.7
  var : sum_products
  49764.8: 0.00020000000000001866
  49726.450000000004: 0.00020000000000001866
  49719.799999999996: 0.00020000000000001866
  49860.6: 0.00020000000000001866
  49887.8: 0.00020000000000001866
  ...
  48583.5: 0.00010000000000000933
  48583.700000000004: 0.00010000000000000933
  48583.799999999996: 0.00010000000000000933
  48519.149999999994: 0.00010000000000000933
  48583.2: 0.00010000000000000933
  mean: 49808.970245004755
  Credible interval (0.95): 47127.7..52306.0
  Percentiles:
  (0.01 46825.850000000006)
  (0.025 47211.15000000001)
  (0.1 48040.40000000001)
  (0.05 47564.9)
  (0.25 48880.549999999996)
  (0.5 49825.25000000001)
  (0.75 50728.1)
  (0.84 51162.1)
  (0.9 51539.84999999999)
  (0.95 52026.20000000001)
  (0.975 52412.0)
  (0.99 52842.15000000001)
  (0.999 53873.100000000006)
  Histogram:
  44994.6  : 1  
  45211.289: 0  
  45427.977: 1  
  45644.666: 2  
  45861.355: 1  
  46078.043: 6  
  46294.732: 15 
  46511.42 : 19 
  46728.109: 30 
  46944.798: 66 
  47161.486: 81 
  47378.175: 130
  47594.864: 183
  47811.552: 175
  48028.241: 275
  48244.93 : 301
  48461.618: 383
  48678.307: 400
  48894.995: 458
  49111.684: 493
  49328.373: 588
  49545.061: 572
  49761.75 : 633
  49978.439: 679
  50195.127: 632
  50411.816: 627
  50628.505: 543
  50845.193: 471
  51061.882: 442
  51278.57 : 416
  51495.259: 311
  51711.948: 288
  51928.636: 207
  52145.325: 158
  52362.014: 138
  52578.702: 86 
  52795.391: 78 
  53012.08 : 43 
  53228.768: 26 
  53445.457: 13 
  53662.145: 12 
  53878.834: 7  
  54095.523: 4  
  54312.211: 3  

  var : p
  #f: 0.9999999999999474
  mean: 0 (0.0)


|#

(displayln "Model 2")
(define sum_products_orig (sum (map (lambda (p r) (* p r)) payroll rank)))
(show "sum_products_orig" sum_products_orig)

(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

    (define payroll_sample (draw-without-replacement (length payroll) payroll))
    (define rank_sample    (draw-without-replacement (length rank) rank))
    
    (define sum_products (sum (map (lambda (p r) (* p r)) payroll_sample rank_sample)))
    ; How often is the sum of products as low as in the original?
    (define p (<= sum_products sum_products_orig))
      
    (list sum_products
          p
          )
        
   )
)


(show-marginals (model2)
                (list  "sum_products"
                       "p"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.95
                    #:show-histogram? #t
                    #:show-percentiles? #t
                    )
