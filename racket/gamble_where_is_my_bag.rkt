#| 

  Where is my bag problem in Racket Gamble.

  From "Probabilistic Reasoning Under Uncertainty with Bayesian Networks"
  https://www.bayesia.com/2018-03-02-probabilistic-reasoning-under-uncertainty

  (Also see The BaysiaLabBook "Bayesian Networks BayesiaLab" v1, page 51f.)

  Travel from Singapore -> Tokyo -> Los Angeles.
  1) The departure of Singapore -> Tokyo is delayed
  2) Just made the Tokyo -> LA trip. There's a 50/50 change the luggage is on the plane to LA.
  3) Arriving at LA, waiting for the luggage in 5 minutes, the luggage has not been seen.
  What is the probability that the bag was in the Tokyo -> LA plane?
  
  Answer: The probability that the bag was with the LA plane is 33% (as in the talk).
  From Judea Pearl: The Book of Why.


  Output:

(bag-on-plane-p 1/2 wait-time 5)
(#f : 2/3 (0.6666666666666666))
(#t : 1/3 (0.3333333333333333))


(bag-on-plane-p 1/2 wait-time 8)
(#f : 5/6 (0.8333333333333334))
(#t : 1/6 (0.16666666666666666))


(bag-on-plane-p 4/5 wait-time 5)
(#t : 2/3 (0.6666666666666666))
(#f : 1/3 (0.3333333333333333))


(bag-on-plane-p 4/5 wait-time 8)
(#f : 5/9 (0.5555555555555556))
(#t : 4/9 (0.4444444444444444))



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (where-is-my-bag bag-on-plane-p wait-time )
  (displayln (list "bag-on-plane-p" bag-on-plane-p "wait-time" wait-time))
  
  (enumerate

    (define max-time 10)

    ;;  We know that the bag was on the plane with 50% probability.
    ;;  Variant: https:;; www.youtube.com/watch?v=c71pnonOMkI&t=1074s it's 80/20.
    (define bag-on-plane (flip bag-on-plane-p))
       
    ;;  We assume uniform time step
    (define t-time (random-integer max-time))
    
    ;;  Probability that the bag is on the carousel given a specific time
    ;;  (if it was on the plane, that is).
    ;;  The probability that the bag is on the carousel on a given time
    ;;  is directly proportional on the time:
    ;;      time / maxTime
    (define bag-on-carousel (if bag-on-plane
                                (flip (/ (* 1 t-time) max-time))
                                #f))
    
    ;;  He waited 5 minutes without seeing the luggage.
    (observe/fail (= t-time wait-time))
    (observe/fail (eq? bag-on-carousel false))

    ;; Is the bag on the plane?
    bag-on-plane)
  )

(show-model (where-is-my-bag 1/2 5))
(newline)
(show-model (where-is-my-bag 1/2 8))
(newline)
(show-model (where-is-my-bag 8/10 5))
(newline)
(show-model (where-is-my-bag 8/10 8))
(newline)



