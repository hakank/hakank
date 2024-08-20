#| 

  Coupon collector's problem in Racket Gamble.

  There are N different collector's cards hidden in a package, but we don't
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

  This model uses set! which simplifies the model. Nice that Gamble supports this!

  But:
  - enumerate does not work, since it yield incorrect result. 
  - it's not fast (e.g. n=100 25.9s)


  * For n=10

(theoretical 29.28968253968254 approx 29.29800757894046)
var : num-runs2
20: 0.04999999999999999
26: 0.04599999999999999
25: 0.04399999999999999
29: 0.04399999999999999
19: 0.04299999999999999
22: 0.04299999999999999
21: 0.041999999999999996
24: 0.041999999999999996
27: 0.03799999999999999
23: 0.03699999999999999
...
69: 0.0009999999999999998
70: 0.0009999999999999998
72: 0.0009999999999999998
75: 0.0009999999999999998
11: 0.0009999999999999998
76: 0.0009999999999999998
77: 0.0009999999999999998
78: 0.0009999999999999998
50: 0.0009999999999999998
55: 0.0009999999999999998
mean: 29.207999999999984
Min: 11 Mean: 29.009 Max: 119 Variance: 133.372919 Stddev: 11.548719366232778
Credible interval (0.84): 15..40


  * For n=100

(theoretical 361665906988008779005537951077603286192775/697203752297124771645338089353123035568 518.737751763962 approx 518.7385850888091)

var : num-runs2
416: 0.008999999999999994
562: 0.007999999999999995
446: 0.007999999999999995
493: 0.007999999999999995
492: 0.007999999999999995
507: 0.007999999999999995
531: 0.006999999999999996
457: 0.006999999999999996
462: 0.006999999999999996
481: 0.006999999999999996
...
962: 0.0009999999999999994
455: 0.0009999999999999994
463: 0.0009999999999999994
985: 0.0009999999999999994
989: 0.0009999999999999994
480: 0.0009999999999999994
1006: 0.0009999999999999994
494: 0.0009999999999999994
499: 0.0009999999999999994
505: 0.0009999999999999994
mean: 518.4699999999995


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
  ; approx (+ (* n (log n)) (* gamma n) 1/2  )
  (* n (for/sum ([i (range n)]) 
         (/ (+ i 1))))
  )

; Approximate probability
; n * log n + n*eulers_gamma + 1/2
(define (cc-approx n)
  (define eulers-gamma 0.5772156649)
  (+ (* n (log n)) (* n eulers-gamma) 1/2)
  )

  

(define (coupon-collectors-problem n)
  (let ([theo (cc-theoretical n)]
        [approx (cc-approx n)]
        )
    (displayln (list "theoretical"  (exact->inexact theo) "approx" approx))
    )
  (; enumerate ; incorrect value!
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define slots (make-vector n 0))

   ; Are all slots filled, i.e. > 0?
   (define (all-filled) (andmap (lambda (i)                          
                                  (> (vector-ref slots i) 0)) (range n)))

   ; Update (vector-set!) the count for a random slot.
   (define (run) (for/last ([i (in-naturals 1)]
                            #:break (all-filled))
     (let ([pos (random-integer n)])
       (vector-set! slots pos (add1 (vector-ref slots pos)))
       i ; This is the i'th run
       )
     ))
   
   (define num-runs (run))

   ; This is not really needed since (run) breaks when
   ; all slots are filled
   (observe/fail (all-filled))
   
   (list num-runs)

   )
  )


(time (show-marginals (coupon-collectors-problem 10)
                (list "num-runs")
                #:num-samples 1000
                #:truncate-output 10
                ; #:show-stats? #t 
                ; #:credible-interval 0.84
                ))
