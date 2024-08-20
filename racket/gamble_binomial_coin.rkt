#| 

  Binomial coin in Racket Gamble.

  From https://reference.wolfram.com/language/ref/BinomialDistribution.html
  """
  Compute the probability that there are between 60 and 80 heads in 100 coin flips.

  Probability[60 <= x <= 80, x -> heads[100]]
  -> 0.028444

  ... and an unfair coin (p:0.6)
  Probability[60 <= x <= 80, x -> uheads[100]]
  -> 0.543289
  
  """

  This is a port of my WebPPL model binomial_coin.wppl

var : num-fair-heads
49: 0.08020000000000019
51: 0.0790000000000002
50: 0.07780000000000023
48: 0.0743000000000002
53: 0.07010000000000019
...
32: 0.0003000000000000008
33: 0.00020000000000000052
66: 0.00010000000000000026
67: 0.00010000000000000026
68: 0.00010000000000000026
mean: 50.03510000000013

var : num-unfair-heads
60: 0.08500000000000021
62: 0.0804000000000002
59: 0.07690000000000019
61: 0.0765000000000002
58: 0.0760000000000002
...
44: 0.0006000000000000016
42: 0.0003000000000000008
43: 0.0003000000000000008
76: 0.00020000000000000052
77: 0.00020000000000000052
mean: 59.92590000000014

var : fair coin (60..80)
#f: 0.9696000000000019
#t: 0.030400000000000045
mean: 0.030400000000000045

var : unfair coin (60..80)
#t: 0.5408000000000008
#f: 0.45920000000000033
mean: 0.5408000000000008

Exact probability via dist-pdf
Fair coin  : 0.028443966685352262
UnFair coin: 0.5432886045570291

Exact probability via dist-cdf
Fair coin  : 0.028443966685352207
Unfair coin: 0.543288604557029


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

(define (binomial-coin)
  (; enumerate ; Too slow (i.e. too large)
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define N 100)
   (define p-fair 1/2)
   (define p-unfair 6/10)   

   ;; Using bernoulli
   (define num-fair-heads (for/sum ([i (range N)])
                            (bernoulli p-fair)))
   
   (define num-unfair-heads (for/sum ([i (range N)])
                              (bernoulli p-unfair)))
   
   (list num-fair-heads
         num-unfair-heads
         (and (>= num-fair-heads   60) (<= num-fair-heads   80))
         (and (>= num-unfair-heads 60) (<= num-unfair-heads 80))
         )
    )
   )

(show-marginals (binomial-coin)
                (list "num-fair-heads" "num-unfair-heads"
                      "fair coin (60..80)" "unfair coin (60..80)")
                #:num-samples 10000
                #:truncate-output 5
                )

(displayln "Exact probability via dist-pdf")
(define (b-pdf p from to)
  (for/sum ([v (range from (add1 to))]) (dist-pdf (binomial-dist 100 p) v)))

(displayln (format "Fair coin  : ~a" (b-pdf 0.5 60 80)))
(displayln (format "UnFair coin: ~a" (b-pdf 0.6 60 80)))

(displayln "\nExact probability via dist-cdf")
(define (b-cdf p val)
  (dist-cdf (binomial-dist 100 p) val))
  
(displayln (format "Fair coin  : ~a" (- (b-cdf 0.5 80) (b-cdf 0.5 59))))
(displayln (format "Unfair coin: ~a" (- (b-cdf 0.6 80) (b-cdf 0.6 59))))
         
