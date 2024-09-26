#| 

  CSI  in Racket Gamble.

  From BLOG example/csi:
  """
  Model that illustrates context-specific independence.  This is a 
  parameterization of the model shown in Fig. 3 of (Milch et al., 
  AISTATS 2005): X depends on W when U is true, but on V when U is false.  
  
  The probability of X being true given no evidence is P(X=true) = 0.596.
  """

  var : X
  #t: 0.5960000000000002
  #f: 0.4039999999999999
  mean: 0.5960000000000002

  This is a port of my WebPPL model csi.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define U (flip 0.3))
   (define V (flip 0.9))
   (define W (flip 0.1))

   ; (U) ? (W == true ? (flip 0.8) : (flip 0.2)) :
   ; (V) ? (flip 0.8) : (flip 0.2); 
   
   (define X
     (if U
         (if W (flip 0.8) (flip 0.2))
         (if V (flip 0.8) (flip 0.2))))
         
    
   (list X)

   )
)

(show-marginals (model)
              (list  "X"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )
