#| 

  Thermometer problem in Racket Gamble.

  From the Hakuru example thermometer.hk
  https://hakaru-dev.github.io/workflow/continuous/

var : m1
mean: 21.112690635403997
Min: -6.10292852647141 Mean: 21.112656136186537 Max: 46.944577606356525 Variance: 39.93533179473705 Stddev: 6.319440781804752
Credible interval (0.84): 12.138680430320468..29.429476016897343

var : m2
mean: 21.06972605529515
Min: -20.075457485247743 Mean: 20.99275358635871 Max: 71.03387921359527 Variance: 70.19731773427728 Stddev: 8.378383957200652
Credible interval (0.84): 9.458806273226745..32.0147855151613

var : noiseT
mean: 5.5123854897178
Min: 3.0003761716834836 Mean: 5.493783108259175 Max: 7.999894686038163 Variance: 2.0848281006236196 Stddev: 1.4438933827065001
Credible interval (0.84): 3.535491778604288..7.717721044152505

var : noiseM
mean: 2.503898331966154
Min: 1.000384164992698 Mean: 2.4989031896347784 Max: 3.9998162644826305 Variance: 0.7405557014858557 Stddev: 0.8605554610168108
Credible interval (0.84): 1.3496691979773328..3.8418939388622393


  This is a port of my WebPPL model taxi.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define noiseT (uniform 3 8))
   (define noiseM (uniform 1 4))
  
   (define t1 (normal 21 noiseT))
   (define t2 (normal t1 noiseT))
  
   (define m1 (normal t1 noiseM))
   (define m2 (normal t2 noiseM))
  
   (list m1
         m2
         noiseT
         noiseM
         ; t1
         ; t2
         )


   )
  )

(show-marginals (model)
                (list "m1"
                      "m2"
                      "noiseT"
                      "noiseM"
                      ; "t1"
                      ; "t2"

                      )
                #:num-samples 10000
                #:truncate-output 2
                #:skip-marginals? #t
                #:credible-interval 0.84
                #:show-stats? #t
                )
