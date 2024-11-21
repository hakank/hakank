#| 

  Golf putting (geometry based I) in Racket/Gamble 

  This is a port of the PyMC3 model (the geometry based model)
  https://docs.pymc.io/pymc-examples/examples/case_studies/putting_workflow.html
  """
  We use a data set from “Statistics: A Bayesian Perspective”, by Don Berry (1995). 
  The dataset describes the outcome of professional golfers putting from a number of distances. 
  """

  var : variance_of_shot
  mean: 0.026644764465401347

  var : post 2 feet
  mean: 1377.8908417882046

  var : post 3 feet
  mean: 568.6733317681039

  var : post 4 feet
  mean: 311.97002466413505

  var : post 5 feet
  mean: 205.48170230595338

  var : post 6 feet
  mean: 135.0664896893457

  var : post 7 feet
  mean: 111.46685302916936

  var : post 8 feet
  mean: 92.20651479654603

  var : post 9 feet
  mean: 74.73193355818461

  var : post 10 feet
  mean: 62.20826914606078

  var : post 11 feet
  mean: 67.65001107814801

  var : post 12 feet
  mean: 53.04483475033605

  var : post 13 feet
  mean: 46.577612249861225

  var : post 14 feet
  mean: 39.67210632045817

  var : post 15 feet
  mean: 35.08753618418997

  var : post 16 feet
  mean: 38.50563193757559

  var : post 17 feet
  mean: 35.93731472890847

  var : post 18 feet
  mean: 33.903769270078

  var : post 19 feet
  mean: 24.855234291521512

  var : post 20 feet
  mean: 23.861026626099594


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


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n (length distances))
   
   ;; (define variance_of_shot1 (clip-distx (normal-dist 0 1) 0 1000))
   ;; (define variance_of_shot (sample variance_of_shot1))
   (define variance_of_shot (normal 0 1))
   
   (define p_goes_in
     (for/list ([i n])
       ; 2 * Phi(Math.asin((CUP_RADIUS - BALL_RADIUS) / distances(i)) / variance_of_shot) - 1
       (let ([t (- (* 2 (Phi (/ (asin (/ (- CUP_RADIUS BALL_RADIUS) (list-ref distances i))) variance_of_shot))) 1)])
         (observe/fail (>= t 0))
         t)
       )
     )
   
   (for ([i n])
     (observe-sample (binomial-dist (list-ref tries i) (list-ref p_goes_in i)) (list-ref successes i)))
   
   ;; Posterior
   (define post (for/list ([i n])
                  (binomial (list-ref tries i) (list-ref p_goes_in i) )))
   
   (list variance_of_shot
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


