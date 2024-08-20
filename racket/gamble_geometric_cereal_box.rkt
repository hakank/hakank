#| 

  Geometric cereal box in Racket Gamble.

  From Mathematica (GeometricDistribution) 
  """
  A cereal box contains one out of a set of n different plastic animals. The animals are equally 
  likely to occur, independently of what animals are in other boxes. Simulate the animal 
  collection process, assuming there are 10 animals for 25 boxes: 

    RandomVariate[DiscreteUniformDistribution[{1, 10}], 25]
    -> 
    {8,5,4,2,2,9,2,5,2,3,4,5,1,10,1,6,8,8,6,10,8,10,4,7,4}

  After k unique animals have been collected, the number of boxes needed to find a new unique 
  animal among the remaining n-k follows a geometric distribution with parameter 1-k/n. Find 
  the expected number of boxes needed to get a new unique animal: 

    e = Expectation[x + 1, x -> GeometricDistribution[1 - k/n]]
    -> 
    -(n/(k - n))

  Number of boxes before next unique animal:
    Block[{n = 5}, DiscretePlot[e, {k, 1, n - 1}, ExtentSize -> 1/2]]

  Find the expected number of boxes needed to collect 6 unique animals:
    
    Block[{n = 5}, Sum[e, {k, 0, n - 1}]]
    -->
    137/12 (11.4167)

  Table of this   

    Table[Sum[e, {k, 0, n - 1}], {n, 0, 10}]
    N@%
    -> 
    {0, 1, 3, 11/2, 25/3, 137/12, 147/10, 363/20, 761/35, 7129/280, 7381/252}
    {0., 1., 3., 5.5, 8.33333, 11.4167, 14.7, 18.15, 21.7429, 25.4607, 29.2897}

  The sum:
    Sum[e, {k, 0, n - 1}]
    ->
    -n (-EulerGamma - PolyGamma[0, 1 + n])

  """

  Also see https://en.wikipedia.org/wiki/Coupon_collector%27s_problem

  This is much faster than gamble_coupon_collectors_problem.rkt 
  (which use a resample approach), especially for larger n.

  Note that enumerate is unusable here since it tries to cover all possible
  combinations (which are infinite).


  * n=10
(theoretical 29.28968253968254 approx 29.29800757894046)
var : v
23: 0.04910000000000003
25: 0.048200000000000034
22: 0.04510000000000003
24: 0.044000000000000025
26: 0.04390000000000003
21: 0.04270000000000003
20: 0.04000000000000003
27: 0.03980000000000003
19: 0.03930000000000003
28: 0.036200000000000024
...
78: 0.00020000000000000015
80: 0.00020000000000000015
85: 0.00020000000000000015
89: 0.00020000000000000015
91: 0.00020000000000000015
10: 0.00010000000000000007
79: 0.00010000000000000007
84: 0.00010000000000000007
96: 0.00010000000000000007
101: 0.00010000000000000007
mean: 29.39080000000002

  * n=100 (about 1.5s using rejection-sampler)
(theoretical 518.737751763962 approx 518.7385850888091)
var : v
476: 0.005300000000000024
443: 0.0050000000000000235
493: 0.004900000000000022
471: 0.004800000000000022
436: 0.004600000000000022
440: 0.004600000000000022
488: 0.004600000000000022
427: 0.0045000000000000205
452: 0.0045000000000000205
407: 0.004400000000000021
...
974: 0.00010000000000000047
973: 0.00010000000000000047
983: 0.00010000000000000047
989: 0.00010000000000000047
999: 0.00010000000000000047
1002: 0.00010000000000000047
1005: 0.00010000000000000047
1014: 0.00010000000000000047
1013: 0.00010000000000000047
1023: 0.00010000000000000047
mean: 518.8654000000015

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

; Approximate probability
; n * log n + n*eulers_gamma + 1/2
(define (cc-approx n)
  (define eulers-gamma 0.5772156649)
  (+ (* n (log n)) (* n eulers-gamma) 1/2)
  )


(define (geometric-cereal-box n)
  (let ([theo (cc-theoretical n)]
        [approx (cc-approx n)]
        )
    (displayln (list "theoretical"  (exact->inexact theo) "approx" approx))
    )
  (; enumerate ; too slow
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; bad!

   ; Note: we calculate for n-1
   (define vv (for/list ([i (range n)])
                (add1 (geometric (- 1 (/ i n))))))

   (define v (sum vv))
   
   (list v)

   )
  )


(time (show-marginals (geometric-cereal-box 10)
                (list "v")
                #:num-samples 10000
                #:truncate-output 10
                ; #:show-stats? #t 
                ; #:credible-interval 0.84
                ))
