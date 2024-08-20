#| 

  Credible interval in Racket Gamble.

  Show the credible interval of samples `my-samples  with
  the credibility mass of `cred-mass`

  This is a port of my WebPPL utility credibleInterval

  Credible intervals are available in the main function 
  - (show-model)
  - (show-marginals)

  Here is an example for (normal 100 15):

((87.84317523421812 91.79651913878301 77.99135393045157 77.80194956322015 146.69403475402558 81.8530918214364 72.35444469588293 112.67544200817257 93.60108458433663 125.03250292408643) ...)
Min: 55.88632049484042 Mean: 100.59499913811105 Max: 159.51131847590167 Variance: 233.01424477424024 Stddev: 15.264804118436642
Credible interval (0.01): 89.08123148498356..89.27391669635715
Credible interval (0.05): 88.94975621394293..90.48742112095381
Credible interval (0.1): 103.47257293827568..107.02426089604904
Credible interval (0.25): 99.62317656619213..109.24961448104835
Credible interval (0.5): 88.73087658538026..108.7082479423587
Credible interval (0.75): 82.99048308082561..118.05600819896105
Credible interval (0.9): 77.99135393045157..127.39462675381654
Credible interval (0.95): 71.15138259627952..128.90457699684615
Credible interval (0.99): 61.579922809569624..141.15392070674267
Credible interval (0.999): 55.88632049484042..159.51131847590167


generate-samples on the binomial model:
Credible interval (0.84): 0..3

(2 : 0.29400000000000004)
(1 : 0.25400000000000006)
(3 : 0.21000000000000002)
(0 : 0.12000000000000001)
(4 : 0.07800000000000001)
(5 : 0.03200000000000001)
(6 : 0.009000000000000001)
(7 : 0.0020000000000000005)
(8 : 0.0010000000000000002)
(mean: 2.0200000000000005)
Min: 0 Mean: 2.018 Max: 6 Variance: 1.431676 Stddev: 1.1965266399040182
Credible interval (0.84): 0..3



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model)
  (rejection-sampler
   
   ; (define p (binomial 10 0.2))
   (define p (poisson 10))
   p)
  )

; (let ([samples (repeat (model) 1000)])
(let ([samples (repeat (lambda () (normal 100 15)) 1000)])
; (let ([samples (repeat (lambda () (poisson 10)) 10000)])
;(let ([samples (repeat (lambda () (binomial 100 0.5)) 10000)])    
  ; (writeln samples)
  ; (show-freq samples)
  ; (newline)
  (displayln (list (for/list ([i (range 10)]) (list-ref samples i)) "..."))
  (show-stats samples)
  (for ([m (list 0.01 0.05 0.1  0.25 0.5  0.75  0.90 0.95 0.99 0.999)])
    (show-credible-interval samples m)
    )
  )
(newline)

;; Using generate-samples instead of "manually" sample it
(displayln "\ngenerate-samples on the binomial model:")
(show-credible-interval (vector->list (generate-samples (model) 1000)) 0.84)
(newline)

;; The credible interval is included by default in (show-model model)
(show-model (model))
(newline)

(displayln "Testing (model) again:")
(define ss (vector->list (generate-samples (model) 10000)))
; (show "ss" ss)
(show "ss distinct" (remove-duplicates ss))
; (define *cred-mass* 0.9999)
(define *cred-mass* 0.5)
; (define *cred-mass* 0.84)


(show-credible-interval ss *cred-mass*)

(show-credible-interval2 ss *cred-mass*)

; Compare with the histogram
(show-histogram ss)


