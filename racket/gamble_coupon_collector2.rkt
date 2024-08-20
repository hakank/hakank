#| 

  Coupon collecter's problem, or card collecter's problem in Racket Gamble.

  There are N different collecter's cards hidden in a package, but we don't
  know which card there is in the package we buy.
  We want to collect all of them, how many packages must one buy to collect
  all the different cards?

  See https://en.wikipedia.org/wiki/Coupon_collector%27s_problem
  """
  In probability theory, the coupon collector's problem describes 'collect all coupons and win' 
  contests. It asks the following question: If each box of a brand of cereals contains a 
  coupon, and there are n different types of coupons, what is the probability that more 
  than t boxes need to be bought to collect all n coupons? 

  An alternative statement is: Given n coupons, how many coupons do you expect you need 
  to draw with replacement before having drawn each coupon at least once? The mathematical 
  analysis of the problem reveals that the expected number of trials needed grows as 
  Θ(n log(n).
  For example, when n = 50 it takes about 225[b] trials on average to collect all 50 coupons. 

  ...

  [b]: E(50) = 50(1 + 1/2 + 1/3 + ... + 1/50) = 224.9603, the expected number of trials to 
  collect all 50 coupons. 

   The approximation n*log(n) + γ*n + 1/2 for this expected number gives in this case 
   ≈ 195.6011 + 28.8608 + 0.5 ≈ 224.9619.  [log is the natural logarithm]
  """ 


  This is a port of my WebPPL model coupon_collector2.wppl

  This version draw random integers until all integers 0..N-1 have been selected.
  We only study the length of the generated array.

  It's fairly good, but is slow for larger n.

  * n=10

(theoretical 29.28968253968254)
var : len
22: 0.048900000000000034
24: 0.046400000000000025
25: 0.044000000000000025
21: 0.04320000000000003
19: 0.04130000000000003
...
92: 0.00010000000000000007
98: 0.00010000000000000007
102: 0.00010000000000000007
104: 0.00010000000000000007
109: 0.00010000000000000007
mean: 29.049100000000013

  * n=50 (9.0s)
(theoretical 224.96026691647126)
var : len
214: 0.01699999999999999
189: 0.013999999999999992
181: 0.01299999999999999
206: 0.01299999999999999
217: 0.01299999999999999
...
455: 0.0009999999999999994
478: 0.0009999999999999994
484: 0.0009999999999999994
235: 0.0009999999999999994
240: 0.0009999999999999994
mean: 224.70099999999982


  * n=100 (31.5s)

(theoretical 518.737751763962)
var : len
415: 0.008999999999999994
471: 0.008999999999999994
435: 0.007999999999999995
475: 0.007999999999999995
484: 0.007999999999999995
...
1014: 0.0009999999999999994
503: 0.0009999999999999994
1019: 0.0009999999999999994
506: 0.0009999999999999994
509: 0.0009999999999999994
mean: 518.8399999999997


  Cf some of these models:
   * gamble_coupon_collectors_problem.rkt
   * gamble_geometric_cereal_box.rkt
   * (and gamble_coupon_collector.rkt but it's not a especially good model)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;"Exact" probability
; from https://en.wikipedia.org/wiki/Coupon_collector%27s_problem (footnote [b])
(define (cc-theoretical n)
  (* n (for/sum ([i (range n)]) 
         (/ (+ i 1))))
  )

(define (coupon-collector2 n)
  (displayln (list "theoretical" (exact->inexact (cc-theoretical n))))
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define (collect lst)
     (if (= (length (remove-duplicates lst)) n)
         lst
         (collect (cons (random-integer n) lst))))

    (define len (length (collect '())))

    (list len)
   
   )
  
  )


(show-marginals (coupon-collector2 100)
                (list "len"
                      )
                #:num-samples 1000
                #:truncate-output 5
                )
