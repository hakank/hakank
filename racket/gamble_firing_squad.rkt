#| 

  Firing squad example (Judea Pearl) Racket Gamble.

  court_order: a court order to shoot a prisoner is given
  captain_signals: captain signal to a and b to shoot the prisoner
  a_shoots: person a shoots at the prisoner
  b_shoots: person b shoots at the prisoner
  death: the prisoner is dead (due to the shooting of a and/or b)

  * no observation

var : court order
#t: 0.7999999999999999
#f: 0.2
mean: 0.7999999999999999

var : captain signals
#t: 0.74
#f: 0.25999999999999995
mean: 0.74

var : a shoots
#t: 0.7289999999999999
#f: 0.271
mean: 0.7289999999999999

var : b shoots
#t: 0.7289999999999999
#f: 0.271
mean: 0.7289999999999999

var : death
#t: 0.7087950000000001
#f: 0.29120499999999994
mean: 0.7087950000000001


  * observation: death

var : court order
#t: 0.9312424607961398
#f: 0.06875753920386013
mean: 0.9312424607961398

var : captain signals
#t: 0.9372738238841978
#f: 0.06272617611580222
mean: 0.9372738238841978

var : a shoots
#t: 0.9256555139356231
#f: 0.07434448606437694
mean: 0.9256555139356231

var : b shoots
#t: 0.9256555139356231
#f: 0.07434448606437694
mean: 0.9256555139356231

var : death
#t: 1.0
mean: 1.0


  * observation: (not captain_signals))

var : court order
#f: 0.6923076923076925
#t: 0.30769230769230765
mean: 0.30769230769230765

var : captain signals
#f: 1.0000000000000002
mean: 0 (0.0)

var : a shoots
#f: 0.9000000000000001
#t: 0.10000000000000013
mean: 0.10000000000000013

var : b shoots
#f: 0.9000000000000001
#t: 0.1000000000000001
mean: 0.1000000000000001

var : death
#f: 0.8290000000000001
#t: 0.1710000000000002
mean: 0.1710000000000002


  * observation: (not b_shoots)

var : court order
#f: 0.6014760147601476
#t: 0.3985239852398525
mean: 0.3985239852398525

var : captain signals
#f: 0.8634686346863467
#t: 0.13653136531365326
mean: 0.13653136531365326

var : a shoots
#f: 0.7839483394833947
#t: 0.21605166051660535
mean: 0.21605166051660535

var : b shoots
#f: 1.0000000000000002
mean: 0 (0.0)

var : death
#f: 0.8055535055350552
#t: 0.19444649446494483
mean: 0.19444649446494483


  This is a port of my WebPPL model firing_squad.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (firing-squad)

  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

    (define court-order (flip 0.8))
    (define captain-signals (if court-order (flip 0.9) (flip 0.1)))
    (define a-shoots (if captain-signals (flip 0.95) (flip 0.1))) 
    (define b-shoots (if captain-signals (flip 0.95) (flip 0.1)))

    (define death
        (if (or a-shoots b-shoots)
            (flip 0.9)
            #f ;; the prisoner don't die if not either a or b shoots
            ))

    (observe/fail death)
    ; (observe/fail (not captain-signals))
    ; (observe/fail (not b-shoots))

    {list court-order captain-signals a-shoots b-shoots death}
    
    )
  )

(show-marginals (firing-squad)
                  (list "court order"
                        "captain signals"
                        "a shoots"
                        "b shoots"
                        "death"
                        )
                  #:num-samples 1000
                  #:truncate-output 5
                  ; #:skip-marginals? #t
                  )


