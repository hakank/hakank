#| 

  Tennis strategy in Racket.Gamble 

  (This is a port of a very old R simulation.)

  In 2003 Niklas Johansson wrote (in Swedish but translated here):
  """
  From when I played Squash I remember that it was best to all time trying to 
  stand in the mid plane where the ball could be placed. Then one get - on 
  average - the least distance to run to hit the ball.
  """

  This model test this by checking two strategies:
  - strategy 0: after hitting the ball, always run back to the middle of the area
  - strategy 1: after hitting the ball, stay in the position until the ball arrives 
    and then run to the new position 

  We start at the mid position, represented at (0,0) and have a 11 x 11 
  game area. Thus the ball can be placed in the square of (-10..10, -10..10).

  The first strategy (strategy 0) will not get lesser _total_ distance
  than the second strategy, infact it will be a longer total run distance
  than for strategy 1.
  However, the mean distance from start position to being able _to hit the
  ball_ is lesser from strategy 0 than for strategy 1. After a strategy 0
  has hit the ball they can then "calmly" get back to position (0,0), while
  a strategy 1 player just stay at position (a2,b2), which would be considered 
  bad time management.

max-distance: 40
var : mean_total_distance0
mean: 19.995082118466716
Credible interval (0.84): 18.488946093269814..21.722635618110232
Percentiles:
(0.01 17.022376048065308)
(0.1 18.509286960291615)
(0.025 17.745147838115408)
(0.05 18.092497274940705)
(0.25 19.221345664476956)
(0.5 20.00691070934698)
(0.75 20.748619767770382)
(0.84 21.083832904751713)
(0.9 21.44176077383715)
(0.95 21.931004005495662)
(0.975 22.35606615311973)
(0.99 22.838528314422337)
(0.999 23.67908436927235)

var : mean_to_hit_distance0
mean: 9.997541059233358
Credible interval (0.84): 9.244473046634907..10.861317809055116
Percentiles:
(0.01 8.511188024032654)
(0.1 9.254643480145807)
(0.025 8.872573919057704)
(0.05 9.046248637470352)
(0.25 9.610672832238478)
(0.5 10.00345535467349)
(0.75 10.374309883885191)
(0.84 10.541916452375856)
(0.9 10.720880386918575)
(0.95 10.965502002747831)
(0.975 11.178033076559865)
(0.99 11.419264157211169)
(0.999 11.839542184636175)

var : ps0
mean: 74.704
Credible interval (0.84): 68..80
Percentiles:
(0.01 63)
(0.1 69)
(0.025 66)
(0.05 67)
(0.25 72)
(0.5 75)
(0.75 78)
(0.84 79)
(0.9 80)
(0.95 82)
(0.975 83)
(0.99 84)
(0.999 87)

var : strat0-better
mean: 0.8740000000000007
Credible interval (0.84): 1..1

var : mean_total_distance1
mean: 13.296584644825822
Credible interval (0.84): 11.995628045008202..14.994632338798494
Percentiles:
(0.01 10.880049586121537)
(0.1 11.984710541511838)
(0.025 11.282082170078786)
(0.05 11.534551423598709)
(0.25 12.591061671762915)
(0.5 13.293723526525893)
(0.75 13.998383278880205)
(0.84 14.363010484330864)
(0.9 14.706308139607332)
(0.95 15.081749028061473)
(0.975 15.415652372607893)
(0.99 15.826928677456733)
(0.999 16.985829922056897)

var : mean_to_hit_distance1
mean: 13.296584644825822
Credible interval (0.84): 11.995628045008202..14.994632338798494
Percentiles:
(0.01 10.880049586121537)
(0.1 11.984710541511838)
(0.025 11.282082170078786)
(0.05 11.534551423598709)
(0.25 12.591061671762915)
(0.5 13.293723526525893)
(0.75 13.998383278880205)
(0.84 14.363010484330864)
(0.9 14.706308139607332)
(0.95 15.081749028061473)
(0.975 15.415652372607893)
(0.99 15.826928677456733)
(0.999 16.985829922056897)

var : ps1
mean: 66.94800000000004
Credible interval (0.84): 61..74
Percentiles:
(0.01 55)
(0.1 61)
(0.025 57)
(0.05 58)
(0.25 63)
(0.5 67)
(0.75 70)
(0.84 72)
(0.9 73)
(0.95 75)
(0.975 76)
(0.99 77)
(0.999 80)

var : strat1-better
mean: 0.09600000000000006
Credible interval (0.84): 0..0

  This simulation shows that strategy 0 is better (in term of successfully hitting 
  the ball) than strategy 1:
   strat0-better: 0.8740000000000007
   strat1-better: 0.09600000000000006


  (Inspired by my Turing.jl model tennis_strategy.jl.)
  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (distance pos1 pos2)
  (let ([d1 (- (first pos1) (first pos2))]
        [d2 (- (second pos2) (second pos2))])
    (sqrt (expt (+ d1 d1) 2))))


(define (model [n 100] [size 10])
  (define max-distance (distance (list (- size) (- size)) (list size size)))
  (show "max-distance" max-distance)
  (flush-output)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   
   (defmem (positions i) (list (uniform (- size) size) (uniform (- size) size)))

   ;
   ; The elements in the returned list ds:
   ; - total run distance
   ; - distance to hit the ball
   ; - probability of hitting the ball. This is to the distance to
   ;   the next position: (- 1 (/ distance max-distance))
   ;
   (defmem (distances i ds this_pos strategy)
     (if (= i 0)
         ds
         (let ([new_pos (positions i)])
           (if (= strategy 0)
               ;; Strategy 0: Run to new position and then back to position (0 0)
               (let* ([dist0 (distance new_pos '(0 0))]
                      [p0 (bernoulli (- 1 (/ dist0 max-distance)))])
                 (distances (sub1 i) (append ds (list (list (* 2 dist0) dist0 p0))) '(0 0) strategy)
                 )
               ;; Strategy 1: Run to the new position and stay there
               (let* ([dist1 (distance this_pos new_pos)]
                      [p1 (bernoulli (- 1 (/ dist1 max-distance)))])
                 (distances (sub1 i) (append ds (list (list dist1 dist1 p1))) new_pos strategy)
                 )
               )
           )))
   
   ; Strategy 0: Back to 0,0
   (define res0 (distances n '() '(0 0) 0))
   (define ds0_total (map first res0))
   (define ds0_to_hit (map second res0))      
   (define ps0 (sum (map third res0)))
   (define mean_total_distance0 (/ (sum ds0_total) n))
   (define mean_to_hit_distance0 (/ (sum ds0_to_hit) n))

   ; Strategy 1: stay at position
   (define res1 (distances n '() '(0 0) 1))
   (define ds1_total (map first res1))
   (define ds1_to_hit (map second res1))   
   (define ps1 (sum (map third res1)))
   (define mean_total_distance1 (/ (sum ds1_total) n))
   (define mean_to_hit_distance1 (/ (sum ds1_to_hit) n))

   (define p-strat0-better (> ps0 ps1))
   (define p-strat1-better (> ps1 ps0))   

   
   (list mean_total_distance0
         mean_to_hit_distance0
         ps0
         p-strat0-better
         mean_total_distance1
         mean_to_hit_distance1         
         ps1
         p-strat1-better
    
         )
   )
)

(show-marginals (model)
                (list "mean_total_distance0"
                      "mean_to_hit_distance0"
                      "ps0"
                      "strat0-better"
                      "mean_total_distance1"
                      "mean_to_hit_distance1" 
                      "ps1"
                      "strat1-better"                      
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                #:show-percentiles? #t
                )


