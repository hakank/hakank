#| 

  Poisson Ball in Racket.Gamble 

  From BLOG example/poisson-ball.blog
  """
  Model file for balls in an urn, allowing observation errors. 
  This version uses a Poisson prior for the number of balls.
  """

var : numBalls
6: 0.16199999999999992
5: 0.1509999999999999
7: 0.14999999999999994
4: 0.13399999999999995
8: 0.11499999999999995
3: 0.08899999999999997
9: 0.07199999999999997
10: 0.045999999999999985
2: 0.03199999999999999
11: 0.027999999999999987
12: 0.009999999999999995
13: 0.007999999999999997
15: 0.001999999999999999
1: 0.0009999999999999996
mean: 6.2349999999999985

var : numTrue
5: 0.2459999999999999
6: 0.18999999999999992
4: 0.18599999999999994
3: 0.12099999999999994
7: 0.11999999999999994
8: 0.06899999999999996
2: 0.03599999999999999
9: 0.014999999999999993
1: 0.013999999999999992
0: 0.001999999999999999
10: 0.0009999999999999996
mean: 5.099999999999998

var : prob
0.5: 0.2459999999999999
0.6: 0.18999999999999992
0.4: 0.18599999999999994
0.3: 0.12099999999999994
0.7: 0.11999999999999994
0.8: 0.06899999999999996
0.2: 0.03599999999999999
0.9: 0.014999999999999993
0.1: 0.013999999999999992
0: 0.001999999999999999
1.0: 0.0009999999999999996
mean: 0.5099999999999998

var : post obs 0
Blue: 1.0000000000000002

var : post obs 1
Green: 1.0000000000000002


  This is a port of my WebPPL model poisson_ball.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define observations
  (list "Blue" "Green" "Blue" "Green" "Blue" "Green" "Blue" "Green" "Blue" "Green"))


(define (model)
  (; enumerate #:limit 1e-03
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define colors (vector "Blue" "Green"))   
   (define n (length observations))
   (define Draw (range n))
   
   (define numBalls (poisson 6))
   
   (defmem (TrueColor b) (categorical-vw2 (vector 0.5 0.5) colors))
   
   (defmem (BallDrawn d) (random-integer numBalls))
   
   (defmem (ObsColor d) 
     (let ([trueColor (TrueColor (BallDrawn d) )])
       (cond [(eq? trueColor "Blue") (categorical-vw2 (vector 0.8 0.2) colors)]
             [(eq? trueColor "Green") (categorical-vw2 (vector 0.2 0.8) colors)]))
     )
   
   ;; Constrain so we don't get 0 balls from Poisson
   (observe/fail (> numBalls 0))
   
   #| 
   """
   Evidence file asserting that the drawn balls appeared blue on half the 
   draws and green on half the draws.
   """
   |#   
   (for ([i n])
     ; (observe/fail (eq? (ObsColor i)) (list-ref observations i)))
     (observe-sample (dist-unit (ObsColor i)) (list-ref observations i)))
   
   (define numTrue (for/sum ([i n]) (if (eq? (ObsColor i) (TrueColor i)) 1 0 )))
     
   (list numBalls
         numTrue
         (* 1.0 (/ numTrue n))
         (ObsColor 0)
         (ObsColor 1)
         )
   
   )
)

(show-marginals (model)
                (list  "numBalls"
                       "numTrue"
                       "prob"
                       "post obs 0"
                       "post obs 1"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


