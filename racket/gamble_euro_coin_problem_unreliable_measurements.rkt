#| 

  The Euro coin problem Racket Gamble.

  From Think Bayes, page 33ff
  """
  A statistical statement appeared in "The Guardian" on Friday January 4, 2002:
      When spun on edge 250 times, a Belgian one-euro coin
      came up heads 140 times and tails 110. 'It looks very
      suspicious to me,' said Barry Blight, a statistics lecturer
      at the London School of Economics. 'If the coin were
      unbiased, the chance of getting a result as extreme as
      that would be less than 7%.'

  But do these data give evidence that the coin is biased rather than fair?
  """
 
  Continues on page 41:
  """
  Exercise 4.1. Suppose that instead of observing coin tosses directly, you measure
  the outcome using an instrument that is not always correct. Specifically, suppose
  there is a probability y that an actual heads is reported as tails, or actual tails re-
  ported as heads.

  Write a class that estimates the bias of a coin given a series of outcomes and the
  value of y .
  How does the spread of the posterior distribution depend on y ?
  """

var : prob
0.5989720203564116: 0.0009999999999999994
0.555429793530535: 0.0009999999999999994
0.5816817595386748: 0.0009999999999999994
0.5307135512590594: 0.0009999999999999994
0.42735231261078443: 0.0009999999999999994
...
0.477939092444281: 0.0009999999999999994
0.4472109784068205: 0.0009999999999999994
0.5906401830240773: 0.0009999999999999994
0.5754210917919321: 0.0009999999999999994
0.6112678817983583: 0.0009999999999999994
mean: 0.5351669427418871

var : prob < 0.5
#f: 0.7880000000000006
#t: 0.21200000000000016
mean: 0.21200000000000016

var : error
#f: 0.8100000000000006
#t: 0.19000000000000014
mean: 0.19000000000000014

 

  This is a port of my WebPPL model euro_coin_problem_unreliable_measurements.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (euro-coin-problem-unreliable-measurements)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; Probability of throwing head
   (define prob (beta 2 2))
   
   ;; We measure incorrect with probability 0.2
   (define error (flip 0.2))
    
   ;; random Integer coin(Integer i) ~ Bernoulli(prob);
   (define (throwCoin i) (bernoulli prob))
    
   (define (coin i)
        (if error
            (- 1 (throwCoin i))
            (throwCoin i)))
   
   (define sum250 (for/sum ([i (range 250)]) (coin i)))
   
   (observe/fail (= sum250 140))

   (list prob
         (< prob 0.5)
         error)
   )
  )

(show-marginals (euro-coin-problem-unreliable-measurements)
                  (list "prob"
                        "prob < 0.5"
                        "error"
                        )
                  #:num-samples 1000
                  #:truncate-output 5
                  ; #:skip-marginals? #t
                  )


