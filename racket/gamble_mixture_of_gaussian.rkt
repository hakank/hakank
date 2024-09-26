#| 

  Mixture of gaussian  in Racket.Gamble 

  From BLOG example/mixture-of-gaussian.blog
  """
  A model for mixture of Gaussian distribution. 
  The latent variable \verb|z| determines the mixture component of \verb|x|. 
  The program asks the possible value of \verb|z| given the evidence of \verb|x| being 0.2.
   
  @author leili
  @since 2014-06-16
  """
 
  var : z
  #t: 0.5497052853588994
  #f: 0.45029471464110066
  mean: 0.5497052853588994


  This is a port of my WebPPL model mixture_of_gaussian.wppl

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

   (define z (flip 0.5))
   (define x (if (= (boolean->integer z) 1)
                 (normal-dist 0.5 1.0)
                 (normal-dist -0.5 1.0)))
    
   (observe-sample x 0.2)
    
   (list z)

   )
)

(show-marginals (model)
                (list  "z"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


