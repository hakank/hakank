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


  This is a port of my WebPPL model coupon_collector.wppl
  Note: This is not a good model since it overshoots the number needed.
        For example, for n=50, the theoretical value is 224.96026691647126,
        but this model give the mean value of 288.1318000000067.
        And n=100 it give a mean of 769.8 instead of the theoretical 518.737751763962.

        I'd guess that using poisson distribution is not a good idea.

  Instead, see some of these models:
   * gamble_coupon_collectors_problem.rkt
   * gamble_geometric_cereal_box.rkt

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

(define (coupon-collector n)
  (displayln (list "theoretical" (exact->inexact (cc-theoretical n))))
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define m (inexact->exact (round (sqrt n))))
     
   (define p (add1 (random-integer m)))
   ; (define p (beta 10 10)) ; for the geometric distribution
   
   ;; Fill this slot with cards
   (define (fill c)  (poisson p))
   
   ; Using a geometric distribution is not better
   ; (define (fill c)  (geometric p))

   ;; How many slots are filled (i.e. > 0)?
   ; (define total-filled (for/sum ([c (range n)]) (if (> (fill c) 0) 1 0)))
   (define total-filled (andmap (lambda (c) (> (fill c) 0))
             (range n)))

   ; (observe/fail (= total-filled n))
   (observe/fail total-filled)
     
     
   (list ; total-filled
         p
         (for/sum ([c (range n)]) (fill c))
         (fill 0)
         ;; (fill 1)
         ;; (fill 2)
         ;; (fill 3)
         ;; (fill 4)
         ;; (fill 5)
         ;; (fill 6)
         ;; (fill 7)
         ;; (fill 8)
         ;; (fill 9)
         )
   )
  
  )


(show-marginals (coupon-collector 100)
                (list ; "total-filled"
                      "p"
                      "sum"
                      "fill 0"
                      ;; "fill 1"
                      ;; "fill 2"
                      ;; "fill 3"
                      ;; "fill 4"
                      ;; "fill 5"
                      ;; "fill 6"
                      ;; "fill 7"
                      ;; "fill 8"
                      ;; "fill 9"
                      )
                #:num-samples 10000
                #:truncate-output 5
                )
