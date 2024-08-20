#| 

  Thermostat problem in Racket Gamble.

  OMEGA: Fast, casual Inference from Simple Parts 
  From  https://www.youtube.com/watch?v=oCvbqKE2tWA
  @ ~ 21min
  
  The Omega (Julia) model is here:
  https://github.com/zenna/Omega.jl/blob/master/docs/src/causal.md

var : this-time
morning: 0.33700000000000024
afternoon: 0.33300000000000024
evening: 0.33000000000000024

var : is_ac_on
#f: 0.7660000000000006
#t: 0.23400000000000018
mean: 0.23400000000000018
Min: 0 Mean: 0.234 Max: 1 Variance: 0.179244 Stddev: 0.423372176695635
Credible interval (0.84): 0..1

var : is_window_open
#t: 0.5070000000000003
#f: 0.4930000000000004
mean: 0.5070000000000003
Min: 0 Mean: 0.498 Max: 1 Variance: 0.249996 Stddev: 0.49999599998399985
Credible interval (0.84): 0..1

var : outside_temp
30.892190273784923: 0.0009999999999999994
29.314723571861713: 0.0009999999999999994
16.637511985453173: 0.0009999999999999994
32.933676896733374: 0.0009999999999999994
...
21.730683605614555: 0.0009999999999999994
21.94273796124835: 0.0009999999999999994
19.718954956130062: 0.0009999999999999994
35.45430751435066: 0.0009999999999999994
mean: 20.65235510422035
Min: 4.539473807801841 Mean: 20.801937315371973 Max: 39.22585200841587 Variance: 87.94024145659186 Stddev: 9.37764583766053
Credible interval (0.84): 9.130865599815774..33.917677042998534

var : room_temp
27.159553967332588: 0.0009999999999999994
24.59647143656296: 0.0009999999999999994
18.952516343367023: 0.0009999999999999994
21.2800750778428: 0.0009999999999999994
...
23.42581801977561: 0.0009999999999999994
27.08736615139471: 0.0009999999999999994
23.211002303216603: 0.0009999999999999994
25.300114824181517: 0.0009999999999999994
mean: 23.852572136679274
Min: 12.959791797737434 Mean: 23.73134213744646 Max: 30.48457038159747 Variance: 8.744984492038613 Stddev: 2.957191994449906
Credible interval (0.84): 19.72470617495974..27.825477184092684

var : thermostat
18.952516343367023: 0.0009999999999999994
21.2800750778428: 0.0009999999999999994
23.79530748970335: 0.0009999999999999994
28.097828420256953: 0.0009999999999999994
...
23.42581801977561: 0.0009999999999999994
27.08736615139471: 0.0009999999999999994
27.632361618601823: 0.0009999999999999994
25.573385929573902: 0.0009999999999999994
mean: 22.753263055506896
Min: 12.959791797737434 Mean: 22.82504544794526 Max: 32.13142171776911 Variance: 17.239373796660157 Stddev: 4.152032489836774
Credible interval (0.84): 17.08807957250709..28.937888940097267


  This is a port of my WebPPL model thermostat.wppl

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

   (define times '("morning" "afternoon""evening"))
    
   (define this-time (uniform-draw times))
    
   (define is_window_open (flip 0.5))
    
   ;; a.c. is off when window is closed
   (define is_ac_on (if is_window_open #f (flip 0.5)))
    
   ;; hottest at noon, cool at night
   (define outside_temp
     (cond
       [(eq? this-time "morning")   (normal 20 2)]
       [(eq? this-time "afternoon") (normal 32 2)]
       [(eq? this-time "evening")   (normal 10 2)]
       [else (normal 25 2)]
       ))
   
   
   ;; a.c. chills the room
   (define room_temp (if is_ac_on (normal 20 2) (normal 25 2)))
    
   
   ;; great insulation
   (define thermostat (if is_window_open (/ (+ outside_temp room_temp) 2.0) room_temp))
    
   ; (observe/fail is_ac_on)
   ; (observe/fail (not is_window_open))
   ; (observe/fail (= room_temp 20.0))
   ; (observe/fail (> room_temp 20.0))
   ; (observe/fail (eq? this-time "evening"))
   ; (observe/fail (> outside_temp 10.0))
   ; (observe/fail (< outside_temp 10.0))   
   ; (observe/fail (> thermostat 30.0))
    
   (list
        this-time
        is_ac_on
        is_window_open
        outside_temp
        room_temp
        thermostat
        )

   )
  )

(show-marginals (model)
                (list "this-time"
                      "is_ac_on"
                      "is_window_open"
                      "outside_temp"
                      "room_temp"
                      "thermostat"

                      )
                #:num-samples 1000
                #:truncate-output 4
                ; #:skip-marginals? #t
                #:credible-interval 0.84
                #:show-stats? #t
                )
