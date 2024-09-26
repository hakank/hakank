#| 

  A/B test simple 2 exact in Racket.Gamble 

  Originally from http://rpubs.com/rasmusab/exercise_2_bayesian_ab_testing
  but with some different values and an exact model.
  
  Two persons (or companies etc) has different number of trials and 
  probability of success:
    (define nA = 25 ;; number of trial for A
    (define sA = 16 ;; number of successes for A
    
    (define nB = 72 ;; number of trial for B
    (define sB = 57 ;; number of successes for B

  pA = 16/25 = 0.64
  pB = 57/72 = 0.79167..

  The question is if A (with the lesser success rate) can manage to be better than B,
  and if so, by how much.


var : pA
16/25: 1.0000000000000178
mean: 0.6400000000000113

var : pB
19/24: 1.0000000000000178
mean: 0.7916666666666807

var : n
57: 0.010000000000001393
74: 0.01000000000000139
67: 0.010000000000001388
38: 0.010000000000001386
42: 0.010000000000001386
...
96: 0.01000000000000134
64: 0.010000000000001334
91: 0.010000000000001332
93: 0.01000000000000133
97: 0.010000000000001319
mean: 50.50000000000687

var : a
6: 0.015625000000002134
1: 0.01562500000000213
3: 0.01562500000000213
4: 0.01562500000000213
2: 0.015625000000002113
...
96: 1.826074442141443e-16
97: 1.2994533197921844e-17
98: 6.87020380418147e-19
99: 2.3989386882596704e-20
100: 4.149515568881572e-22
mean: 32.32000000000441

var : b
1: 0.012631578947370168
4: 0.012631578947370159
9: 0.012631578947370159
13: 0.012631578947370159
10: 0.012631578947370152
...
96: 1.6439104489419133e-8
97: 2.4408804510669493e-9
98: 2.6972733491436605e-10
99: 1.9715763963462962e-11
100: 7.148845711942655e-13
mean: 39.97916666667208

var : diff
1: 0.06441411613644311
2: 0.06377662512635764
3: 0.06293135132039626
4: 0.06183583072029907
5: 0.060448174613395564
...
-96: 4.813010021907859e-81
-97: 4.438711920005144e-83
-98: 3.04672457610878e-85
-99: 1.383604694529995e-87
-100: 3.117982410208437e-90
mean: 7.659166666667709

var : p
#f: 0.9440294518382414
#t: 0.05597054816182882
mean: 0.05597054816182882


  This is a port of my WebPPL model ab_test_simple2_exact.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define nA 25) ; number of trial for A
   (define sA 16) ; number of successes for A
    
   (define nB 72) ; number of trial for B
   (define sB 57) ; number of successes for B
    
   (define pA (/ sA nA))
   (define pB (/ sB nB))

   (define n (add1 (random-integer 100)))
   (define a (binomial n pA))
   (define b (binomial n pB))
    
   (define diff (- b a))
   (define p (> a b))

   ;; Some experiments
   ;; (observe/fail p == true) ;; ensure that B wins
   ;; (observe/fail diff == 0)
   
   (list  pA
          pB
          n
          a
          b
          diff
          p
          )
   )
)

(show-marginals (model)
                (list  "pA"
                       "pB"
                       "n"
                       "a"
                       "b"
                       "diff"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


