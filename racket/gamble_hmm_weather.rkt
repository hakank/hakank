#| 

  Hidden Markov model - weather in Racket.Gamble 

  From ProbLog example hmm_weather.pl
  """
  % System test 6 - a Hidden Markv Model.
  % Description: An HMM which represents the state of the weather every day based on the 
  % previous observations. 
  % Query: what is the probability of sunny weather in 10 days.
  % Expected outcome: 
  % weather(sun,10) 0.3333508096

  %% At time T=0
  0.5::weather(sun,0) ; 0.5::weather(rain,0) <- true.

  %% Time T>0
  0.6::weather(sun,T) ; 0.4::weather(rain,T) <- T>0, Tprev is T-1, weather(sun,Tprev).
  0.2::weather(sun,T) ; 0.8::weather(rain,T) <- T>0, Tprev is T-1, weather(rain,Tprev).

  %%% Queries
  query(weather(sun,10)).
  %query(weather(rain,0)).
  """

  Weather where the last day is 0..10:

  t: 0
  var : weather last day
  rain: 0.5
  sun: 0.5

  t: 1
  var : weather last day
  sun: 1.0

  t: 2
  var : weather last day
  sun: 0.5999999999999999
 rain: 0.4

  t: 3
  var : weather last day
  rain: 0.56
  sun: 0.43999999999999995

  t: 4
  var : weather last day
  rain: 0.6240000000000001
  sun: 0.37599999999999983

  t: 5
  var : weather last day
  rain: 0.6496000000000002
  sun: 0.3503999999999997

  t: 6
  var : weather last day
  rain: 0.6598399999999995
  sun: 0.34016000000000046

  t: 7
  var : weather last day
  rain: 0.6639360000000001
  sun: 0.33606400000000003

  t: 8
  var : weather last day
  rain: 0.6655744
  sun: 0.33442560000000005

  t: 9
  var : weather last day
  rain: 0.6662297599999994
  sun: 0.3337702400000005

  t: 10
  var : weather last day
  rain: 0.6664919040000019
  sun: 0.33350809599999803

  Reaching 1/3 eventually.



  This is a port of my Turing.jl model hmm_weather.jl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (model [T 10])
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler ; #:transition (slice)

   (define sun "sun")
   (define rain "rain")
   
   (defmem (weather t)
     (categorical-vw2 (vector 0.5 0.5) (vector sun rain)))
   
   ; First day is sunny
   (observe/fail (eq? (weather 0) sun))
   
   (for ([t (range 1 T)])
     (observe/fail (eq? (weather t)
                        (if (eq? (weather (sub1 t)) sun)
                            (categorical-vw2 (vector 0.6 0.4) (vector sun rain))
                            (categorical-vw2 (vector 0.2 0.8) (vector sun rain))))))
   
   ; Weather the last day
   (list (weather (sub1 T)))
   
   )
  )

(for ([t 11])
  (show "t" t)
  (show-marginals (model t)
                (list  "weather last day"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.93
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
)



#|
  Trying to recover the transition table. 
  Nope, this don't work as expected

|#

#|
(displayln "\nTrying to recover the transition table sun:(0.6 0.4) rain:(0.2 0.8)")
(define *samples* (flatten (make-samples (model 10) 10 #:num-internal-samples 1000)))
(show "samples" *samples*)
(show-freq *samples*)
(newline)

; Restoring the transition table 
(define (model2 weather)

  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler ; #:transition (slice)
   
   (define T (length weather))

  
   (define sun "sun")
   (define rain "rain")

   (define (conv v)
     (case v
       [("sun") 1]
       [("rain") 2]))
   
   (define p_sun (dirichlet (vector 0.5 0.5)))
   (define p_rain (dirichlet (vector 0.5 0.5)))   
     
   (for ([t (range 1 T)])
     ;; (observe/fail (eq? (list-ref weather t)
     ;;                    (if (eq? (list-ref weather (sub1 t)) sun)
     ;;                        (categorical-vw2 p_sun (vector sun rain))
     ;;                        (categorical-vw2 p_rain (vector sun rain))))))
     ; For normal-dist, convert to numeric values 
     (observe-sample (normal-dist (conv (list-ref weather t)) 0.1)
                     (conv (if (eq? (list-ref weather (sub1 t)) sun)
                               (categorical-vw2 p_sun (vector sun rain))
                               (categorical-vw2 p_rain (vector sun rain))))
                        ))
  
   (list (vector-ref p_sun 0)
         (vector-ref p_sun 1)
         (vector-ref p_rain 0)
         (vector-ref p_rain 1)
         ; p_sun
         ; p_rain
         )

   )
  )

(show-marginals (model2 *samples*)
                (list  "p_sun 0"
                       "p_sun 1"
                       "p_rain 0"
                       "p_rain 1"
                       "p_sun"
                       "p_rain"
                       )
                #:num-samples 1000
                #:truncate-output 1
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.93
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
|#
