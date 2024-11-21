#| 

  Zipf dist (max values) in Racket/Gamble 

  Note: the maximum possible value is n.

  Compare with 
  > (zipf_quantile 10 0.3 0.999999999)
  10

  The distribution of the max value depends very much on how many 
  samples there are in the list. 
  
num-samples: 1
variable : z-max
1: 0.4318
2: 0.1772
3: 0.1057
4: 0.0743
5: 0.0536
6: 0.0464
7: 0.0357
8: 0.0272
9: 0.0257
10: 0.0224
mean: 2.8697000000000004

num-samples: 3
variable : z-max
2: 0.1477
3: 0.1425
4: 0.1233
5: 0.1095
6: 0.0987
7: 0.0847
1: 0.083
8: 0.0744
9: 0.0719
10: 0.0643
mean: 4.917

num-samples: 10
variable : z-max
10: 0.2041
9: 0.1811
8: 0.1631
7: 0.143
6: 0.116
5: 0.0908
4: 0.0607
3: 0.0322
2: 0.0085
1: 0.0005
mean: 7.483599999999998

num-samples: 20
variable : z-max
10: 0.3576
9: 0.2656
8: 0.1763
7: 0.1076
6: 0.058
5: 0.0258
4: 0.0079
3: 0.001
2: 0.0002
mean: 8.642000000000001

num-samples: 40
variable : z-max
10: 0.5866
9: 0.2695
8: 0.1058
7: 0.0305
6: 0.0064
5: 0.0011
4: 0.0001
mean: 9.395699999999998


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(for ([s (range 0 2)])
  (show "s" s)
  (show "(zipf_quantile 10 0.3 0.999999999)" (zipf_quantile 3 s 0.999999999))
  (show "(zipf_mean 10 0.3)" (* 1.0 (zipf_mean 3 s)))
  )

(define (model num-samples)
  (show "num-samples" num-samples)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define num-samples 10)
   (define n 10)
   (define s 0.3)
   (define z (for/list ([i num-samples]) (zipf n s)))
   (define z-max (max-list z))
  
   (list z-max
         ; z
         )
   
   )
)

(for ([num-samples '(1 3 10 20 40)])
  (show-marginals (model num-samples)
                  (list  "z-max"
                         "z"
                         )
                  #:num-samples 10000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:hpd-interval (list 0.84)
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ; #:burn 0
                  ; #:thin 0
                    )
  )
