#| 

  Grass in Racket.Gamble 

  This is a port of the R2 model Grass.cs
  
  We observe that the grass is wet. Did it rain?
  
  Output from the R2 model:
  """
  Mean: 0.699
  Variance: 0.21061
  Number of accepted samples = 790
  """

  var : rain
  mean: 0.7079276773296245

  var : cloudy
  mean: 0.5757997218358831

  var : sprinkler
  mean: 0.4297635605006957

  var : temp1
  mean: 0.7000000000000001

  var : wetRoof
  mean: 0.4955493741307372

  var : temp2
  mean: 0.9582753824756608

  var : temp3
  mean: 0.9304589707927681


  This is a port of my WebPPL model grass.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define cloudy (flip 0.5))

   (define rain (if cloudy (flip 0.8) (flip 0.2)))
   (define sprinkler (if cloudy (flip 0.1) (flip 0.5)))
   (define temp1 (flip 0.7))
   (define wetRoof (and temp1 rain))

   (define temp2 (flip 0.9))
   (define temp3 (flip 0.9))

   (define wetGrass (or (and temp2 rain) (and temp3 sprinkler)))

   ;; We observe that the grass is wet.
   ;;  Did it rain?
   (observe/fail wetGrass)

   (list rain
         cloudy
         sprinkler
         temp1
         wetRoof
         temp2
         temp3
         )
   
   )
)

(show-marginals (model)
                (list "rain"
                      "cloudy"
                      "sprinkler"
                      "temp1"
                      "wetRoof"
                      "temp2"
                      "temp3"
                     )
                    #:num-samples 1000
                    ; #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.9
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )
