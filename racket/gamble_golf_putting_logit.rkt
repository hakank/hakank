#| 

  Golf putting (logit) in Racket/Gamble 

  This is a port of the PyMC3 model (the Logic model)
  https://docs.pymc.io/pymc-examples/examples/case_studies/putting_workflow.html
  """
  We use a data set from “Statistics: A Bayesian Perspective”, by Don Berry (1995). 
  The dataset describes the outcome of professional golfers putting from a number of distances. 
  """

  PyMC3 model:
     mean      std 
  - a: -0.255 	0.007
  - b:  2.224 	0.060 	

  Well, this is not very impressive...

  var : a
  mean: -0.15829966211450194

  var : b
  mean: 0.6817167973887809

  var : post 2 feet
  mean: 690.8807000001682

  var : post 3 feet
  mean: 324.06054000007947

  var : post 4 feet
  mean: 205.35690000005113

  var : post 5 feet
  mean: 155.60502000003896

  var : post 6 feet
  mean: 116.31922000002933

  var : post 7 feet
  mean: 106.49582000002695

  var : post 8 feet
  mean: 96.53877000002441

  var : post 9 feet
  mean: 85.18367000002166

  var : post 10 feet
  mean: 76.03450000001929

  var : post 11 feet
  mean: 87.17624000002209

  var : post 12 feet
  mean: 71.78768000001824

  var : post 13 feet
  mean: 66.35277000001676

  var : post 14 feet
  mean: 58.05912000001477

  var : post 15 feet
  mean: 53.92425000001369

  var : post 16 feet
  mean: 63.20383000001601

  var : post 17 feet
  mean: 59.157390000015084

  var : post 18 feet
  mean: 55.84660000001403

  var : post 19 feet
  mean: 41.43974000001043

  var : post 20 feet
  mean: 41.339390000010475

  Note: The importance-sampler crashes because of +nan.0 as probabilities.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

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

;; From https://discourse.julialang.org/t/help-with-first-non-trivial-turing-example/38964
;; logit = x -> log(x / (1 - x))
(define (invlogit x)
  (/ (exp x) (+ 1 (exp x))))

(define (logit x) (log (/ (- 1 x))))

(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler
   
   (define n (length distances))
    
   (define a (normal 0 1))
   (define b (normal 0 1))
   (define (p i) (invlogit (+ (* a (list-ref distances i) b))))
  
   (for ([i n]) 
     (observe-sample (binomial-dist (list-ref tries i) (p i)) (list-ref successes i)))

   ; Posterior
   (define post (for/list ([i n]) (binomial (list-ref tries i) (p i))))

  
   (list a
         b
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
                (list  "a"
                       "b"
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
                    #:num-samples 100000
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


