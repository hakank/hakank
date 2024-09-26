#| 

  Meeting under the clock in Racket.Gamble 

  """
  Meeting Under the Clock (This problem is posed by Julian Simon(1994))

  Two persons agree to arrive at the two clock sometime between 1 pm and 2 pm 
  and to stay for 20 minutes. What is the probability that they will be there
  at the same time?
  """

  var : c1
  mean: 61/2 (30.5)

  var : c2
  mean: 61/2 (30.5)

  var : d
  mean: 3599/180 (19.994444444444444)

  var : prob
  #t: 17/30 (0.5666666666666667)
  #f: 13/30 (0.43333333333333335)
  mean: 17/30 (0.5666666666666667)


  This is a port of my WebPPL model meeting_under_the_clock .wppl

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
  (enumerate ; #:limit 1e-05
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define wait_time 20)
   (define c1 (add1 (random-integer 60)))
   (define c2 (add1 (random-integer 60)))   

   ;; (define prob = c1 > c2 ? c1 - c2 <= wait_time : c2-c1 <= wait_time;
   (define d (abs (- c1 c2)))
   (define prob (<= d wait_time))
   
   (list c1
         c2
         d
         prob
         )
   
   )
)

(show-marginals (model)
              (list  "c1"
                     "c2"
                     "d"
                     "prob"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


