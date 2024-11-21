#| 

  Simulated quantiles for distributions in Racket/Gamble 

  quantiles(dist,qs,n)

  Get the qs'th quantiles of a distribution dist (using n samples),

  Example: 
    What are the (0.90,0.95,0.99,0.999)'th quantiles for binomial(1/365,90) using 10000 samples:
    console.log(quantiles(function() ( return binomial(1/1000,90)),(0.90,0.95,0.99,0.999),10000))
    ->  ( 0, 1, 1, 2 )

  Note: 
  - dist much be a distribution wrapped in a function() ( ... )  
  - very small / large quantiles normally requires quite a few samples.


  This is a port of my WebPPL model quantiles_test.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define num-samples 1000)
(define qs '(0.001  0.01 0.02 0.05 0.10 0.25 0.50 0.75 0.90 0.95 0.98 0.99 0.999))

(define (print-list lst)
  (for ([el lst])
    (displayln el)))

(displayln "normal 100 15")
(print-list (dist-quantiles (lambda () (normal 100 15)) qs num-samples))
(newline)
(displayln "binomial 10 0.49")
(print-list (dist-quantiles (lambda () (binomial 10 0.5)) qs num-samples))
(newline)
(displayln "frechet_dist 10 5")
(print-list (dist-quantiles (lambda () (frechet_dist 10 5)) qs num-samples))
(newline)
(displayln "frechet_dist 10 5 q=0.5")
(print-list (dist-quantiles (lambda () (frechet_dist 10 5)) '(0.5) num-samples))
(newline)
(displayln "poisson 10")
(print-list (dist-quantiles (lambda () (poisson 10)) qs 10000))
(displayln "Compare with poisson-dist 10")
(for ([q qs])
  (displayln (list q (dist-inv-cdf (poisson-dist 10) q)))
  )
(newline)
