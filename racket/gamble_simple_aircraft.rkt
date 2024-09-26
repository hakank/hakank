#| 

  Simple aircraft in Racket.Gamble 

  From BLOG example/simple-aircraft.blog


  var : numAircraft
  1: 0.7137
  2: 0.2552
  3: 0.0291
  4: 0.002
  mean: 1.3194

  var : sumBlip
  3: 1.0
  mean: 3.0


  This is a port of my WebPPL model simple_aircraft.wppl.


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; Number of Aircrafts
   (define numAircraft (poisson 5))

   ;; #Blip(Source=a) ~ Poisson(4); ;; BLOG
   ;; I.e. each Aircraft has a blip
   (defmem (numBlip a) (poisson 4))

   (define sumBlip (for/sum ([a numAircraft]) (numBlip a)))
  
   (observe/fail (> numAircraft 0))
   (observe-sample (dist-unit sumBlip) 3)

   (list  numAircraft
          sumBlip
          )
    
   )
)

(show-marginals (model)
                (list  "numAircraft"
                       "sumBlip"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


