#| 

  Mixture of gaussian in Racket.Gamble 

  From BLOG examples/mixture-of-gaussian-infinite.blog
  """
  Gaussian mixture model with infinite number of components
 
  @author leili
  @date 2014-07-10
  """

  var : x 0
  mean: 0.30809393013976466
  Credible interval (0.84): -1.1532073925501247..1.8817768360560905

  var : x 1
  mean: 0.37931890107936045
  Credible interval (0.84): -0.8887200845238199..1.9783190806904147

  var : x 2
  mean: 0.32345995803147687
  Credible interval (0.84): -1.1324546572754177..1.8903831882651194

  var : x 3
  mean: 0.34534967281121237
  Credible interval (0.84): -1.0670472755887859..1.974672142730451

  var : x 4
  mean: 0.32093016088133264
  Credible interval (0.84): -1.1218164145950114..1.9217355863919459

  var : numComponents
  mean: 2.2005748858908607
  Credible interval (0.84): 1..3

  var : z 0
  mean: 0.5972521666023839
  Credible interval (0.84): 0..1


  This is a port of my WebPPL model mixture_of_gaussian_infinite.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(require racket/set) ; for set-intersect

(define (model)
  (; enumerate ; #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define numComponent (poisson 2))
    
   (defmem (z i) (random-integer numComponent))
    
   (defmem (meanx c) (uniform -1 1))
    
   (defmem (x i) (normal-dist (meanx (z i)) 1.0))

   ;; Remove all size numComponent 0 cases (as in the BLOG model)
   (observe/fail (> numComponent 0))
    
   (observe-sample (x 0) 0.2)
   (observe-sample (x 1) 1.2)
   (observe-sample (x 2) 0.5)
   (observe-sample (x 3) 0.6)
   
   (list (sample (x 0))
         (sample (x 1))
         (sample (x 2))
         (sample (x 3))
         (sample (x 4)) ; predication         
         numComponent
         (z 0)
    )
  
   )
)

(show-marginals (model)
                (list  "x 0"
                       "x 1"
                       "x 2"
                       "x 3"
                       "x 4"
                       "numComponents"
                       "z 0"                       
                       )
                #:num-samples 10000
                #:truncate-output 3
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


