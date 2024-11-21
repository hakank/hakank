#| 

  Tests of geometric dist* in Racket/Gamble 

  Comparing the three implementations of geometric distributions
  in gamble_distributions.rkt

  Generate (all three implementations)
  '(1 0 4 2 16 1 5 10 10 11)
  '(0 2 2 5 6 3 7 2 20 10)
  '(8 12 6 3 35 13 13 0 2 5)
  Mean (generated)
  9.05472
  8.98981
  9.07128
  Percentiles (generated)
  '((0.01 0) (0.025 0) (0.1 1) (0.05 0) (0.25 2) (0.5 6) (0.75 13) (0.84 17) (0.9 21) (0.95 28) (0.975 35) (0.99 44) (0.999 65))
  '((0.01 0) (0.025 0) (0.1 1) (0.05 0) (0.25 2) (0.5 6) (0.75 13) (0.84 17) (0.9 21) (0.95 28) (0.975 34) (0.99 43) (0.999 64))
  '((0.01 0) (0.025 0) (0.1 1) (0.05 0) (0.25 2) (0.5 6) (0.75 13) (0.84 17) (0.9 21) (0.95 28) (0.975 35) (0.99 44) (0.999 69))
  PDF
  43046721/1000000000
  0.04304672100000001
  CDF
  612579511/1000000000
  0.6125795109999999
  Quantile
  (quantile  0 0)
  (quantile  0.1 1)
  (quantile  0.2 2)
  (quantile  0.30000000000000004 3)
  (quantile  0.4 4)
  (quantile  0.5 6)
  (quantile  0.6 8)
  (quantile  0.7 11)
  (quantile  0.7999999999999999 15)
  (quantile  0.8999999999999999 21)
  (quantile  0.9999999999999999 348)
  (quantile 0.95  28)
  (quantile 0.99  43)
  Mean
  9


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(displayln "Generate (all three implementations)")
(repeat (lambda () (geometric_dist 1/10)) 10)
(repeat (lambda () (geometric_dist2 1/10)) 10)
(repeat (lambda () (geometric_dist3 1/10)) 10)

(displayln "Mean (generated)")
(* 1.0 (avg (repeat (lambda () (geometric_dist 1/10)) 100000)))
(* 1.0 (avg (repeat (lambda () (geometric_dist2 1/10)) 100000)))
(* 1.0 (avg (repeat (lambda () (geometric_dist3 1/10)) 100000)))

(displayln "Percentiles (generated)")
(percentiles (repeat (lambda () (geometric_dist 1/10)) 100000))
(percentiles (repeat (lambda () (geometric_dist2 1/10)) 100000))
(percentiles (repeat (lambda () (geometric_dist3 1/10)) 100000))

(displayln "PDF")
(geometric_dist_pdf 1/10 8)
(geometric_dist_pdf 1/10 8.0)
(displayln "CDF")
(geometric_dist_cdf 1/10 8)
(geometric_dist_cdf 1/10 8.0)

(displayln "Quantile")
(for ([q (range 0 1 0.1)])
  (show2 "quantile " q (geometric_dist_quantile 1/10 q))
  )
(show2 "quantile 0.95 " (geometric_dist_quantile 1/10 0.95))
(show2 "quantile 0.99 " (geometric_dist_quantile 1/10 0.99))

(displayln "Mean")
(geometric_dist_mean 1/10)
  
