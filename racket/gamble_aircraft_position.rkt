#| 

  Aircraft position in  Racket Gamble.

  From BLOG example/aircraft-position.blog

var : numAircraft
3: 0.6079718139082155
4: 0.3009567435693974
5: 0.07628991850123062
6: 0.013035590142910284
7: 0.0015064915178010301
8: 0.00023944236044519675
mean: 3.4998665288740196

var : sumBlips
3: 1.0
mean: 3.0

var : numberAircraftInPosition>5
1: 0.4222545614318873
0: 0.28360604817236157
2: 0.2299313343319838
3: 0.05744731400652885
4: 0.006301810866385256
5: 0.00045893119085329377
mean: 1.081961071535249


  This is a port of my WebPPL model aircraft_position.wppl 
 
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

   
   (define numAircraft (poisson 5))
    
   (defmem (blip a) (bernoulli 0.9))
   
   (define sumBlips (for/sum ([b numAircraft]) (blip b)))
   
   (defmem (position a) (normal 0 10))
   (define positions  (for/list ([a numAircraft]) (position a)))
   
   (defmem (obsPos b) (normal (position (blip b)) 1))
   
   (define (inRange x y epsilon)
     (if (and (> x (- y epsilon)) (< x (+ y epsilon))) 1 0)
     )
    
   (define epsilon 0.05)
   
   ;; Evidence
   (observe-sample (normal-dist (inRange (obsPos 0) 5.0 epsilon) 1) 1)
   (observe/fail (= sumBlips 3))
   
   (define numberAircraftInPosition>5 (for/sum ([a numAircraft])
                                        (boolean->integer (> (position a) 5))))
   
   (list numAircraft
         sumBlips
         numberAircraftInPosition>5
    )
   
   )
  )

(show-marginals (model)
                  (list "numAircraft"
                        "sumBlips"
                        "numberAircraftInPosition>5"
                        )
                  #:num-samples 100000
                  ; #:truncate-output 1
                  ; #:skip-marginals? #t
                  ; #:credible-interval 0.94
                  ; #:show-stats? #t
                  ; #:show-histogram? #t
                  )

