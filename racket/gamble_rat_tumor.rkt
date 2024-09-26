#| 

  Rat tumor in Racket.Gamble 

  PyMC3 hierachical binomial model: Rat Tumor example in
  https://docs.pymc.io/pymc-examples/examples/generalized_linear_models/GLM-hierarchical-binominal-model.html
  """
  This short tutorial demonstrates how to (...) do inference for the rat tumour example found in chapter 
  5 of Bayesian Data Analysis 3rd Edition.
  """
  
  From the PyMC3 model:
  """
  # estimate the means from the samples
  trace("ab").mean(axis=0)
  
  array(( 2.3851397 , 14.18267752))
  """

  var : x
  mean: 2.9412168299097727
  Credible interval (0.94): 2.849477290495383..2.990699409289843

  var : z
  mean: 3.162318260216099
  Credible interval (0.94): 3.02587627350072..3.145569656228869

  var : ab 0
  mean: 4.823494480164384
  Credible interval (0.94): 3.3333108867119683..3.516295104028979

  var : ab 1
  mean: 18.966332950280073
  Credible interval (0.94): 17.27874771400344..19.899595587541324

  

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;; rat data (BDA3, p. 102)
(define y '(0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  1  1
               1  1  1  1  1  2  2  2  2  2  2  2  2  2  1  5  2
               5  3  2  7  7  3  3  2  9 10  4  4  4  4  4  4  4
               10  4  4  4  5 11 12  5  5  6  5  6  6  6  6 16 15
               15  9  4))
(define n '(20 20 20 20 20 20 20 19 19 19 19 18 18 17 20 20 20
               20 19 19 18 18 25 24 23 20 20 20 20 20 20 10 49 19
               46 27 17 49 47 20 20 13 48 50 20 20 20 20 20 20 20
               48 19 19 19 22 46 49 20 20 23 19 22 20 20 20 52 46
               47 24 14))


(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define N (length y))
   
   (define ab (for/list ([i 2]) (uniform 0.5 20)))
    
   (define x (log (/ (first ab) (/ (first ab) (second ab)))))
   (define z (log (sum ab)))
   (define theta (for/list ([i N]) (beta (first ab) (second ab))))
   
   (for ([i N])
     (observe-sample (binomial-dist (list-ref n i) (list-ref theta i)) (list-ref y i)))
   
   (list x
         z
         (first ab)
         (second ab)
         )
   
   )
)

(show-marginals (model)
                (list  "x"
                       "z"
                       "ab 0"
                       "ab 1"
                     )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.94
                ; #:credible-interval2 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


