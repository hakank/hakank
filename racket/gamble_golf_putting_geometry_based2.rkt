#| 

  Golf putting (geometry based II) in Racket/Gamble 

  This is a port of the PyMC3 model (the second geometry based model)
  https://docs.pymc.io/pymc-examples/examples/case_studies/putting_workflow.html
  """
  We use a data set from “Statistics: A Bayesian Perspective”, by Don Berry (1995). 
  The dataset describes the outcome of professional golfers putting from a number of distances. 
  """

  var : variance_of_shot
  mean: 0.06274046956145427

  var : variance_of_distance
  mean: 0.020824874102971177

  var : post 2 feet
  mean: 984.6183000000489

  var : post 3 feet
  mean: 389.29240000001937

  var : post 4 feet
  mean: 204.84700000001018

  var : post 5 feet
  mean: 130.51560000000663

  var : post 6 feet
  mean: 85.4574000000043

  var : post 7 feet
  mean: 71.1377000000036

  var : post 8 feet
  mean: 58.97850000000299

  var : post 9 feet
  mean: 48.054500000002406

  var : post 10 feet
  mean: 40.38480000000202

  var : post 11 feet
  mean: 44.489200000002214

  var : post 12 feet
  mean: 32.98830000000167

  var : post 13 feet
  mean: 29.698200000001492

  var : post 14 feet
  mean: 25.699800000001304

  var : post 15 feet
  mean: 21.913100000001098

  var : post 16 feet
  mean: 25.58240000000129

  var : post 17 feet
  mean: 24.06430000000122

  var : post 18 feet
  mean: 22.32890000000113

  var : post 19 feet
  mean: 15.984100000000801

  var : post 20 feet
  mean: 15.72700000000078

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(require (only-in math/special-functions
                  erf))

;; distance_feet, num_tries, num_successes
(define data '((2 1443 1346) 
               (3 694 577) 
               (4 455 337) 
               (5 353 208) 
               (6 272 149) 
               (7 256 136) 
               (8 240 111) 
               (9 217 69) 
               (10 200 67) 
               (11 237 75) 
               (12 202 52) 
               (13 192 46) 
               (14 174 54) 
               (15 167 28) 
               (16 201 27) 
               (17 195 31) 
               (18 191 33) 
               (19 147 20) 
               (20 152 24)))

(define data_len (length data))
(define distances (map first data))
(define tries     (map second data))
(define successes (map third data))


#|
Calculates the standard normal cumulative distribution function.
|#
(define (Phi x)
    (+ 0.5 (* 0.5 (erf (/ x (sqrt 2.0))))))
 

(define BALL_RADIUS (/ (/ 1.68 2) 12))
(define CUP_RADIUS (/ (/ 4.25 2) 12))
(define OVERSHOT 1.0)
(define DISTANCE_TOLERANCE 3.0)


(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler ; throws on +nan.0 probabilities
   mh-sampler

   (define n (length distances))
   
   (define variance_of_shot (normal 0 1))
   (define variance_of_distance (uniform 0 1))
   
   ; This was p_goes_in in gamble_golf_putting_geometry_based1.rkt
   (define p_good_angle 
     (for/list ((i n))
       ; 2 * Phi(Math.asin((CUP_RADIUS - BALL_RADIUS) / distances(i)) / variance_of_shot) - 1
       (let ((t (- (* 2 (Phi (/ (asin (/ (- CUP_RADIUS BALL_RADIUS) (list-ref distances i))) variance_of_shot))) 1)))
         (observe/fail (>= t 0))
         t)
       )
     )

   (define p_good_distance
     (for/list ([i n]) 
       ;  Phi((DISTANCE_TOLERANCE - OVERSHOT) / ((distances(i) + OVERSHOT) * variance_of_distance))
       ;    - Phi(-OVERSHOT / ((distances(i) + OVERSHOT) * variance_of_distance))
       (- (Phi (/ (- DISTANCE_TOLERANCE OVERSHOT) (* (+ (list-ref distances i) OVERSHOT) variance_of_distance)))
          (Phi (/ (- OVERSHOT) (* (+ (list-ref distances i) OVERSHOT) variance_of_distance)))
          )))
  
   (for ((i n))
     (observe-sample (binomial-dist (list-ref tries i) (* (list-ref p_good_angle i) (list-ref p_good_distance i))) (list-ref successes i)))
   
   ;; Posterior
   (define post
     (for/list ((i n))
       (binomial (list-ref tries i) (* (list-ref p_good_angle i) (list-ref p_good_distance i)))))

   
   (list variance_of_shot
         variance_of_distance
         ; Posterior successes for each distance
         (list-ref post 0) ; 2 feet
         (list-ref post 1) ; 3 feet
         (list-ref post 2)
         (list-ref post 3)
         (list-ref post 4)
         (list-ref post 5)
         (list-ref post 6)
         (list-ref post 7)
         (list-ref post 8)
         (list-ref post 9)
         (list-ref post 10)
         (list-ref post 11)
         (list-ref post 12)
         (list-ref post 13)
         (list-ref post 14)
         (list-ref post 15)
         (list-ref post 16)
         (list-ref post 17)
         (list-ref post 18)
         )        

   )
)

(show-marginals (model)
                (list  "variance_of_shot"
                       "variance_of_distance"
                       "post 2 feet"
                       "post 3 feet"
                       "post 4 feet"
                       "post 5 feet"
                       "post 6 feet"
                       "post 7 feet"
                       "post 8 feet"
                       "post 9 feet"
                       "post 10 feet"
                       "post 11 feet"
                       "post 12 feet"
                       "post 13 feet"
                       "post 14 feet"
                       "post 15 feet"
                       "post 16 feet"
                       "post 17 feet"
                       "post 18 feet"
                       "post 19 feet"
                       "post 20 feet"

                       "p"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    #:burn 1000
                    #:thin 10
                    )


