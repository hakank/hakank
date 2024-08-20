#| 

  BUGS book, 3.4.1 in Racket Gamble.
  Example 3.4.1 Three coins (page 45)
  """
  Suppose I have 3 coins in my pocket. The coins may be either fair, biased 3:1 in
  favour of heads, or 3:1 in favour of tails, but I don't know how many of
  each type there are among the 3 coins. I randomly select 1 coin att toss it once,
  observing a head. What is the posterior distribution of the probability of a head?
  """

var : coin
3: 0.5
2: 0.3333333333333334
1: 0.16666666666666669
mean: 2.3333333333333335

var : (coin_prob 1)
#f: 0.8333333333333335
#t: 0.16666666666666669
mean: 0.16666666666666669

var : (coin_prob 2)
#f: 0.6666666666666667
#t: 0.3333333333333334
mean: 0.3333333333333334

var : (coin_prob 3)
#f: 0.5000000000000001
#t: 0.5
mean: 0.5

var : theta_true
0.75: 0.5
0.5: 0.3333333333333334
0.25: 0.16666666666666669
mean: 0.5833333333333334

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
   
   (observe/fail y 1) ;; We observe a head.

   (list coin (coin_prob 1) (coin_prob 2) (coin_prob 3) theta_true)

   )
  )

(show-marginals (bugs-book-3-4-1b)
                (list "coin" "(coin_prob 1)" "(coin_prob 2)" "(coin_prob 3)" "theta_true")
                )

