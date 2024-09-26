#| 

  Growth in yeast culture in Racket.Gamble 

  From A First Course in Mathematical Modeling, 4th edition,
  page 10ff

  Δp(n) = p(n+1) - p(n) = k * (capacity - p(n)) * p(n)
  p(n+1) = p(n) + k * (capacity - p(n))*p(n) 
  capacity ~ 665
  k        ~ 0.00082


var : k
mean: 0.0008983287217081858

var : capacity
mean: 641.4340642600317

var : v
mean: 6.229417403352483

var : post1
mean: 19.282096997759076

var : post2
mean: 33.43042210319804

var : post3
mean: 52.84444351372696

var : post4
mean: 78.85235275869641

var : post5
mean: 113.59080537107414

var : post6
mean: 179.9893725856962

var : post7
mean: 253.9924727500715

var : post8
mean: 353.87329662463617

var : post9
mean: 446.88017040979076

var : post10
mean: 526.9262900717778

var : post11
mean: 579.9381589480988

var : post12
mean: 606.5007486944723

var : post13
mean: 625.6712572147629

var : post14
mean: 642.6035255198057

var : post15
mean: 648.7590064995585

var : post16
mean: 650.1627117283851

var : post17
mean: 655.8244621695527

var : post18
mean: 656.2736773734781

Credible intervals of capacity:
Credible interval (0.93): 604.5297134533014..792.5734219269996


  This is a port of my WebPPL model growth_in_yeast_culture.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ;; In hours
   (define yeast '(9.6 18.3 29.0 47.2 71.1 119.1 174.6 257.3 350.7 441.0
                       513.3 559.7 594.8 629.4 640.8 651.1 655.9 659.6 661.8))
   (define yeast_mean (avg yeast))
   (define n (length yeast))

   (define k (uniform 0 1))
   (define capacity (uniform 0 1000))
   (define v (normal 5 1))
   (observe/fail (>= v 0)) ;; truncated
   ;; (define v = uniform(0,10)
  
   (for ([t (range 1 n)])
     (let ([yeastt1 (list-ref yeast (sub1 t))])
       (observe-sample (normal-dist (+ yeastt1 (* k (- capacity yeastt1) yeastt1)) v) (list-ref yeast t))))
   
   ;; Posterior check
   (defmem (post t)
     (if (= t 0)
         0
         (let ([yeastt1 (list-ref yeast (sub1 t))])
           (normal (+ yeastt1 (* k (- capacity yeastt1) yeastt1) v)))))

   (list k
         capacity
         v
         ;; Posterior
         (post 1)
         (post 2)
         (post 3)
         (post 4)
         (post 5)
         (post 6)
         (post 7)
         (post 8)
         (post 9)
         (post 10)
         (post 11)
         (post 12)
         (post 13)
         (post 14)
         (post 15)
         (post 16)
         (post 17)
         (post 18)
         )
         
   )
)

(show-marginals (model)
                (list "k"
                      "capacity"
                      "v"
                      "post1"
                      "post2"
                      "post3"
                      "post4"
                      "post5"
                      "post6"
                      "post7"
                      "post8"
                      "post9"
                      "post10"
                      "post11"
                      "post12"
                      "post13"
                      "post14"
                      "post15"
                      "post16"
                      "post17"
                      "post18"
                      
                      )
                #:num-samples 1000
                ; #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.9
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


(displayln "Credible intervals of capacity:")
(show-credible-interval (make-samples (model) 30 #:ix 1) 0.93)

