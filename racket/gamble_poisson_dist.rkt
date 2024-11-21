#| 

  Poisson dist in Racket/Gamble 



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define (model)
  (; enumerate #:limit 1e-14
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

    (define n 4)
    
    (define p (poisson n)) ; Built-in
    (define p2 (poisson_dist n))

    (list p
          p2
          )
   )
)

(show-marginals (model)
                (list  "p"
                       "p2"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.94 0.99 0.999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


