#| 

  Cycling time example in Racket Gamble.

  CyclingTime1 Example from
  "Infer.NET 101 A sample-based introduction to the basics of 
  Microsoft Infer.NET programming", page 12ff.
  """
  averageTimePosterior: Gaussian(15.29, 1.559)
  trafficNoisePosterior: Gamma(1.458, 0.3944)[mean=0.5751]
  ...
  Tomorrows predicted time: 15.29 plus or minus 2.66
  ...
  Probability that the trip takes less than 18 min: 0.85
  """

  var : averageTime
  mean: 15.34424782416623

  var : trafficNoise
  mean: 1.7702965278278464

  var : tomorrowsTime
  mean: 15.37146455462641

  var : probTripTakesLongerThan18Minutes
  mean: 0.922384169656048

  This is a port of my WebPPL model cycling_time.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (cycling-time)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

    (define averageTime (normal 15 10))
    (define trafficNoise (gamma 2.0 1/2))
    
    (define travelTimeMonday (normal-dist averageTime trafficNoise))
    (define travelTimeTuesday (normal-dist averageTime trafficNoise))
    (define travelTimeWednesday (normal-dist averageTime trafficNoise))
    (define tomorrowsTime (normal-dist averageTime trafficNoise))
    
    (define probTripTakesLongerThan18Minutes (< (sample tomorrowsTime) 18.0))
    
    (observe-sample travelTimeMonday 13.0)
    (observe-sample travelTimeTuesday 17.0)
    (observe-sample travelTimeWednesday 16.0)
    
    (list averageTime
          trafficNoise
          (sample tomorrowsTime)
          probTripTakesLongerThan18Minutes
          )
   )
  )

(show-marginals (cycling-time)
                  (list "averageTime"
                        "trafficNoise"
                        "tomorrowsTime"
                        "probTripTakesLongerThan18Minutes"
                        )
                  #:num-samples 10000
                  #:truncate-output 1
                  ; #:skip-marginals? #t
                  #:show-histogram? #t
                  )
