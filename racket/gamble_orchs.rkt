#| 

  Orch problem in Racket Gamble.

  From Church
  """
  ;;; Rejection sampling
  (define (take-sample)
    (define Legolas (random-integer 21))
    (define Gimli (random-integer 21))
    (define Eowyn (random-integer  21))
    (define total-orcs (+ Legolas Gimli Eowyn))
    (if (>= total-orcs 45) Gimli (take-sample)))
  (hist (repeat 1000 take-sample) "Number of Orcs Gimli Took Out, Given That Everyone Took Out More Than 45")
  """

  Result:
rejection-sampler:
(19 : 0.159)
(20 : 0.153)
(18 : 0.134)
(17 : 0.114)
(15 : 0.08)
(14 : 0.079)
(16 : 0.077)
(13 : 0.061)
(12 : 0.045)
(11 : 0.032)
(10 : 0.027)
(8 : 0.017)
(9 : 0.015)
(6 : 0.004)
(7 : 0.003)
(mean: 16.24)
Min: 5 Mean: 16.14 Max: 20 Variance: 10.8524 Stddev: 3.2942981043008235
Credible interval (0.84): 13..20


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")


(define take-sample
  ; (displayln "take-sample")
  (rejection-sampler
   ; importance-sampler ; error
   ; mh-sampler  ; error
   
   (define Legolas (discrete-uniform 21))
   (define Gimli (discrete-uniform 21))
   (define Eowyn (discrete-uniform 21))
   (define total-orcs (+ Legolas Gimli Eowyn))
   ; (displayln (list Legolas Gimli Eowyn "total-orcs" total-orcs))
   (define res (if (>= total-orcs 45) Gimli (take-sample)))

   res
   )
  )

(displayln "rejection-sampler:")
;;: (show-discrete-dist (sampler->discrete-dist take-sample 1000))
;; (exact->inexact (sampler->mean take-sample 1000))
; (show-freq (repeat take-sample 1000))
(show-model take-sample)
(newline)

;
; This shows different results each time. Why?
;
;; (displayln "enumerate:")
;; (show-discrete-dist
;;  (enumerate
  
;;   (define Legolas (discrete-uniform 21))
;;   (define Gimli (discrete-uniform 21))
;;   (define Eowyn (discrete-uniform 21))
;;   (define total-orcs (+ Legolas Gimli Eowyn))
;;   ; (displayln (list Legolas Gimli Eowyn "total-orcs" total-orcs))
;;   (define res (if (>= total-orcs 45) Gimli (take-sample)))
  
;;   res
;;   )
;;  )


