#| 

   Battalion problem in Racket Gamble.


   "First-Order Probabilistic Inference"
   https://www.youtube.com/watch?v=leIqVD4-Fks
   Time 45:06

   (The shown model in the talks is a BLOG model).

   This is a port of my WebPPL model battalion.wppl

   Output:

var : num-battalion
2: 0.1310000000000001
7: 0.11900000000000008
4: 0.10600000000000007
10: 0.10100000000000006
8: 0.09800000000000006
3: 0.09700000000000006
...
1: 0.09500000000000006
6: 0.08900000000000005
9: 0.08500000000000005
5: 0.07900000000000004
mean: 5.393000000000004
Min: 1 Mean: 5.551 Max: 10 Variance: 8.401399 Stddev: 2.8985166896190195
Credible interval (0.84): 1..9

var : large 0
#t: 0.6130000000000004
#f: 0.3870000000000003
mean: 0.6130000000000004
Min: 0 Mean: 0.588 Max: 1 Variance: 0.242256 Stddev: 0.49219508327491446
Credible interval (0.84): 0..1

var : region 0
5: 0.10600000000000007
2: 0.10500000000000007
3: 0.10100000000000006
4: 0.09200000000000005
6: 0.09000000000000005
...
16: 0.004999999999999997
17: 0.0039999999999999975
19: 0.0009999999999999994
20: 0.0009999999999999994
22: 0.0009999999999999994
mean: 6.342000000000001
Min: 1 Mean: 6.551 Max: 18 Variance: 13.949399 Stddev: 3.7348894227272647
Credible interval (0.84): 1..10

var : num-soldier 0
300: 0.04400000000000001
493: 0.01299999999999999
515: 0.010999999999999992
1494: 0.010999999999999992
512: 0.009999999999999993
...
1492: 0.0009999999999999994
464: 0.0009999999999999994
467: 0.0009999999999999994
1527: 0.0009999999999999994
1535: 0.0009999999999999994
mean: 1071.4349999999986
Min: 300 Mean: 1066.146 Max: 1621 Variance: 253266.822684 Stddev: 503.2562197171536
Credible interval (0.84): 471..1538

var : num-soldiers-per-battalions
(300): 0.01299999999999999
(1522): 0.0039999999999999975
(1499): 0.0019999999999999987
(473): 0.0019999999999999987
(1492): 0.0019999999999999987
...
(503 485 488 1476): 0.0009999999999999994
(482 1574 1443 541 1498 1527): 0.0009999999999999994
(480 521 1454 1571 1520 512): 0.0009999999999999994
(1497 494 1429): 0.0009999999999999994
(1537 1458 468 1505 1430): 0.0009999999999999994


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (battalion)
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; slower than importance-sampler

   (define num-battalion (add1 (random-integer 10)))

   (define (large bat) (flip 0.6))
   
   (define (region bat) (add1 (poisson num-battalion)))
   ; (show "region 0" (region 0))
     
   (define (num-soldier bat)
     (if (large bat )
         (poisson 1500)
         (if (= (region bat) 2)
             300
             (poisson 500))))
   
   ; (show "num-soldier 0" (num-soldier 0))

   (define num-soldiers-per-battalions (for/list ([bat (range num-battalion)])
                                         (num-soldier bat)))
   
   (list num-battalion
         (large 0)
         (region 0)
         (num-soldier 0)
         num-soldiers-per-battalions)
   
   )
  )

(show-marginals (battalion)
                (list "num-battalion" "large 0" "region 0" "num-soldier 0" "num-soldiers-per-battalions")
                #:truncate-output 5
                #:show-stats? #t
                #:credible-interval 0.84
                )
