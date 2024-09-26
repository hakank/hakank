#| 

  Chi square inverse distribution (generating samples) in Racket.Gamble 

  From Handbook on probability distributions
  page 81ff


  var : g
  0.19213955532861549: 0.00010000000000000938
  0.12179897041708548: 0.00010000000000000938
  1.908504236213186: 0.00010000000000000938
  0.44550158533696044: 0.00010000000000000938
  1.85631953665311: 0.00010000000000000938
  ...
  0.9929878052960678: 0.00010000000000000938
  0.746843604259409: 0.00010000000000000938
  0.2599123762080537: 0.00010000000000000938
  0.35244142314782534: 0.00010000000000000938
  0.47741074620181695: 0.00010000000000000938
  mean: 0.5029880649408968
  Min: 0.04276211269437963 Mean: 0.5025572882435726 Max: 43.85977368568928 Variance: 0.9506997595042364 Stddev: 0.9750383374535775

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define k 4)
   (define g (chi_squared_inverse_dist k))

   (list g)
   
   )
)

(show-marginals (model)
              (list  "g"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


