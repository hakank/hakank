#| 

  HPD intervals in Racket/Gamble 

  Using math/statistics'' real-hpd-interval
  Perhaps replacing credible interval?

  https://docs.racket-lang.org/math/stats.html#%28def._%28%28lib._math%2Fstatistics..rkt%29._real-hpd-interval%29%29

  Example: HPD-intervals 0.5, 0.84, 0.94, and 0.99 for (normall 100 15). 
  It seems to give exactly the same result as Credible interval, but it
  (presumably) faster.

  var : d
  94.20615003472857: 0.00010000000000000938
  110.41774690110296: 0.00010000000000000938
  109.54316368334966: 0.00010000000000000938
  120.05937378713895: 0.00010000000000000938
  67a.46479206133: 0.00010000000000000938
  ...
  92.56476283217144: 0.00010000000000000938
  109.26080631271299: 0.00010000000000000938
  116.53396113492313: 0.00010000000000000938
  98.73989879863602: 0.00010000000000000938
  107.76064373100222: 0.00010000000000000938
  mean: 99.90321469365034
  Credible interval (0.84): 78.05743576349279..120.77153010858233
  HPD interval (0.5): 90.63682551029557..111.29115185627928
  HPD interval (0.84): 78.05743576349279..120.77153010858233
  HPD interval (0.93): 72.81511522562435..127.02344855195241
  HPD interval (0.99): 61.7992018274258..137.24210118633226

  var : p
  #f: 0.9916999999999472
  #t: 0.008300000000000796
  mean: 0.008300000000000796

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define *samples* (repeat (lambda () (binomial 100 0.3)) 100000))
(show-hpd-interval *samples* 0.84)
(newline)

(define (model)
  (; enumerate #:limit 1e-04
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define d (normal 100 15))
   (define p (>= d 136))

   (list d
         p
         )
   
   )
)

(show-marginals (model)
                (list  "d"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:credible-interval2 0.84
                #:hpd-interval (list 0.5 0.84 0.93 0.99)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


