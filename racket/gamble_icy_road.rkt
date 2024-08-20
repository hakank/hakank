#| 

  Icy road in Racket Gamble.

  From Hugin's icy_roads.net
  """
  This example shows d-separation in action. The risks of Holmes and Watson crashing are 
  only dependent the state of icy is not known.
 
  If it is known, that the roads are icy, they both have a large risk of crashing. Likewise, 
  if the roads are not icy they both have a small risk of crashing.

  But if the state of icy is not known, and Holmes crashes, the risk of Watson crashing 
  goes up, since the crash of Holmes indicates that the roads may be icy.
  """

  Some different tests.
test: 0
no observation
var : icy
mean: 0.7

var : watson
mean: 0.59

var : holmes
mean: 0.59


test: 1
(observe/fail watson)
var : icy
mean: 0.9491525423728814

var : watson
mean: 1.0

var : holmes
mean: 0.764406779661017


test: 2
(observe/fail (not watson))
var : icy
mean: 0.3414634146341463

var : watson
mean: 0 (0.0)

var : holmes
mean: 0.3390243902439024


test: 3
(observe/fail holmes)
var : icy
mean: 0.9491525423728814

var : watson
mean: 0.764406779661017

var : holmes
mean: 1.0


test: 4
(observe/fail (not holmes))
var : icy
mean: 0.3414634146341463

var : watson
mean: 0.3390243902439024

var : holmes
mean: 0 (0.0)


test: 5
(observe/fail (and holmes watson))
var : icy
mean: 0.9933481152993348

var : watson
mean: 1.0

var : holmes
mean: 1.0


test: 6
(observe/fail (and (not holmes) (not watson)))
var : icy
mean: 0.10332103321033208

var : watson
mean: 0 (0.0)

var : holmes
mean: 0 (0.0)


test: 7
(observe/fail icy)
var : icy
mean: 1.0

var : watson
mean: 0.8

var : holmes
mean: 0.8


test: 8
(observe/fail (not icy))
var : icy
mean: 0 (0.0)

var : watson
mean: 0.10000000000000003

var : holmes
mean: 0.10000000000000003

  This is a port of my WebPPL model icy_road.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model test)
  (case test
     [(0) (displayln "no observation")]    
     [(1) (displayln "(observe/fail watson)")]
     [(2) (displayln "(observe/fail (not watson))")]     
     [(3) (displayln "(observe/fail holmes)")]
     [(4) (displayln "(observe/fail (not holmes))")]
     [(5) (displayln "(observe/fail (and holmes watson))")]
     [(6) (displayln "(observe/fail (and (not holmes) (not watson)))")]               
     [(7) (displayln "(observe/fail icy)")]
     [(8) (displayln "(observe/fail (not icy))")])
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define icy (flip 0.7))
      
   ;; This node gives the risk of Watson crashing in his car, given the state of the roads.
   (define watson (if icy (flip 0.8) (flip 0.1)))
    
   ;; This node gives the risk of Holmes crashing in his car, given the state of the roads.
   (define holmes (if icy (flip 0.8) (flip 0.1)))

   (case test
     [(0) '()]    
     [(1) (observe/fail watson)]
     [(2) (observe/fail (not watson))]     
     [(3) (observe/fail holmes)]
     [(4) (observe/fail (not holmes))]
     [(5) (observe/fail (and holmes watson))]
     [(6) (observe/fail (and (not holmes) (not watson)))]
     [(7) (observe/fail icy)]
     [(8) (observe/fail (not icy))])

   ; (observe/fail icy)
   ; (observe/fail watson)
   ; (observe/fail (not watson))
    
   (list icy watson holmes)
 
   )
  )

(for ([test (range 0 9)])
  (show "\ntest" test)
  (show-marginals (model test)
                  (list "icy"
                        "watson"
                        "holmes"
                        )
                  #:num-samples 10000
                  #:truncate-output 1
                  #:skip-marginals? #t
                  )
  )

