#| 

  BUGS book, 3.4.1 in Racket Gamble.

  Example 3.4.1 (b) Three coins, with a prediction.
  """
  Suppose I have 3 coins in my pocket. The coins may be either fair, biased 3:1 in
  favour of heads, or 3:1 in favour of tails, but I don't know how many of
  each type there are among the 3 coins. I randomly select 1 coin att toss it once,
  observing a head. What is the posterior distribution of the probability of a head?
  """

var : y_diff
0.25: 0.375
0.5: 0.16666666666666666
-0.5: 0.16666666666666666
-0.25: 0.12500000000000003
-0.75: 0.12499999999999996
0.75: 0.041666666666666685
mean: 2.0816681711721685e-17

var : y_pred
1: 0.5833333333333333
0: 0.41666666666666663
mean: 0.5833333333333333

var : coin
3: 0.49999999999999994
2: 0.3333333333333333
1: 0.1666666666666667
mean: 2.333333333333333

var : theta_true
0.75: 0.49999999999999994
0.5: 0.3333333333333333
0.25: 0.1666666666666667
mean: 0.5833333333333333

var : (coin_prob 1)
#f: 0.8333333333333333
#t: 0.1666666666666667
mean: 0.1666666666666667

var : (coin_prob 2)
#f: 0.6666666666666667
#t: 0.3333333333333333
mean: 0.3333333333333333

var : (coin_prob 3)
#f: 0.5
#t: 0.49999999999999994
mean: 0.49999999999999994


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

(define (bugs-book-3-4-1b)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 3)
    
   (define coin (add1 (random-integer 3)))
   (define (theta i) 
        (when (and (>= i 1) (<= i n))
          (* 0.25 i)))

   (define theta_true (theta coin))
   (define y (bernoulli theta_true))
   (define (coin_prob i) (= coin i)) ;; Is this the coin?
   (define y_pred (bernoulli theta_true))
   (define y_diff (- y_pred theta_true))
    
   (observe/fail y 1) ;; We observe a head.

   (list y_diff y_pred coin theta_true (coin_prob 1) (coin_prob 2) (coin_prob 3))

   )
  )

(show-marginals (bugs-book-3-4-1b)
                (list "y_diff" "y_pred" "coin" "theta_true" "(coin_prob 1)" "(coin_prob 2)" "(coin_prob 3)")
                )

