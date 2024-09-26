#| 

  Two dimensional mixture model in Racket.Gamble 

  From SPPL model two-dimensional-mixture-model.pynb
  SPPL model:
  """
  X ~= norm(loc=0, scale=2)
  Y ~= 0.6*norm(loc=8, scale=1) | 0.4*gamma(loc=-3, a=3)
  """
  It seems that 0.6 and 0.6 are the weights, i.e. 
  the probability of each cluster 

  The SPPL model returns the following probabilities for these 
   conditions  
                (((-4 < X) < 4) & ((1 < Y) < 2)) 
                (((-4 < X) < 4) & ((-2 < Y) < -1)) 
                (((-1 < X) < 1) & ((-1 <= Y) <= 1)) 
                (((-1 < X) < 1) & ((2 <= Y) < 6))

   for t1..t4:
   prior:
   (0.09278584524638006, 0.04331568961023601, 0.06717622976807626, 0.023365316112895024)
   posterior
   (0.40939191677247666, 0.1911185175795767, 0.29639656127801434, 0.10309300436993234)

  This Gamble model:


  * prior
  var : cluster
  mean: 1.392000000000016

  var : x
  mean: 0.007507521918981281

  var : y
  mean: 4.846978960505459

  var : t1
  mean: 0.08960000000000688

  var : t2
  mean: 0.04080000000000398

  var : t3
  mean: 0.06620000000000621

  var : t4
  mean: 0.022700000000002208


  * posterior

  var : cluster
  mean: 1.9761999999999

  var : x
  mean: -0.013426531693214266

  var : y
  mean: 0.012817349792976662

  var : t1
  mean: 0.4155000000000106

  var : t2
  mean: 0.1865000000000182

  var : t3
  mean: 0.29490000000002387

  var : t4
  mean: 0.10310000000000727


  This is a port of my WebPPL model two_dimensional_mixture_model.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define x (normal 0 2))
   
   ;; Probability of the clusters
   (define cluster (categorical-vw2 (vector 0.6 0.4) (vector 1 2))) ;; (cluster1, cluster2)
   (define y (if (= cluster 1)  (normal 8 1) (- (gamma 3 1) 3)))
   
   (observe/fail (or (and (< -4 x) (< x 4) (< 1 (expt y 2)) (< (expt y 2) 4))
                     (and (< -1 x) (< x 1) (< -1.5 y)       (< y 6))))
   
   ;; The different conditions in the SPPL model
   (define t1 (and (< -4 x) (< x 4) (< -2 y)  (< y -1)))
   (define t2 (and (< -4 x) (< x 4) (< 1 y)   (< y 2)))
   (define t3 (and (< -1 x) (< x 1) (<= -1 y) (<= y 1)))
   (define t4 (and (< -1 x) (< x 1) (<= 2 y)  (< y 6)))
   
   (list cluster
         x
         y
         t1
         t2
         t3
         t4
         )
   
   )
)

(show-marginals (model)
              (list  "cluster"
                     "x"
                     "y"
                     "t1"
                     "t2"
                     "t3"
                     "t4"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


