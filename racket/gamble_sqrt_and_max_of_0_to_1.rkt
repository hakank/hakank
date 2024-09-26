#| 

  Sqrt and max of 0..1 (is the same) in Racket.Gamble 

  From Stand-up Maths
  "The unexpected probability result confusing everyone"
  https://www.youtube.com/watch?v=ga9Qk38FaHM

  The video shows that 
    (max (uniform 0 1) (uniform 0 1))
  is the same (i.e. behaves the same) as
    (sqrt (uniform 0 1))

  The percentiles are quite similar:

  var : v-max
  mean: 0.6666940988236717
  Percentiles:
  (0.01 0.09892384139265843)
  (0.1 0.3161992758441366)
  (0.025 0.1599890702119392)
  (0.05 0.225850230775971)
  (0.25 0.5001054974323939)
  (0.5 0.707936200837309)
  (0.75 0.8667991269133563)
  (0.84 0.9175436729213885)
  (0.9 0.9497073456968945)
  (0.95 0.9753073656149052)
  (0.975 0.9877042461751223)
  (0.99 0.9949809950674063)
  (0.999 0.9995746172292905)

  var : v-sqrt
  mean: 0.6664950487857637
  Percentiles:
  (0.01 0.09796919152727504)
  (0.1 0.3167613778611133)
  (0.025 0.15856469467637016)
  (0.05 0.22428223403138337)
  (0.25 0.5018532987201313)
  (0.5 0.7076307455421188)
  (0.75 0.8664909515747058)
  (0.84 0.9166437742168237)
  (0.9 0.949192872082373)
  (0.95 0.9750829977266654)
  (0.975 0.9877448314119879)
  (0.99 0.9950710279788667)
  (0.999 0.9995712055974927)


  Note that the two 50th percentiles are 0.707936200837309 and 0.7076307455421188 respectively,
  which are close to (sqrt 1/2) = 0.7071067811865476.

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

   (define x (uniform 0 1))
   (define y (uniform 0 1))

   (define v-max (max x y))
   (define v-sqrt (sqrt x))

   (list ; x
         ; y
         v-max
         v-sqrt
         )

   )
  )

(displayln "Model 1")
(show-marginals (model)
                (list  ; "x"
                       ; "y"
                       "v-max"
                       "v-sqrt"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                #:show-percentiles? #t
                )


#|
  Using (random-integer n) instead of (uniform 0 1)
  Note that for the sqrt version we have to use (ceiling (* n n))

  Here's some percentiles for n = 6

Model 2
var : v-max
mean: 4.471920000000001
Percentiles:
(0.01 1)
(0.025 1)
(0.1 2)
(0.05 2)
(0.25 3)
(0.5 5)
(0.75 6)
(0.84 6)
(0.9 6)
(0.95 6)
(0.975 6)
(0.99 6)
(0.999 6)

var : v-sqrt
mean: 4.472060000000001
Percentiles:
(0.01 1)
(0.025 1)
(0.1 2)
(0.05 2)
(0.25 3)
(0.5 5)
(0.75 6)
(0.84 6)
(0.9 6)
(0.95 6)
(0.975 6)
(0.99 6)
(0.999 6)


  But that's not very interesting snce it's too few values to consider. 
 
  Here's for n=20 which show very similar (in fact identical) percentages:

  Model 2
  var : v-max
  mean: 13.797833333333333
  Percentiles:
  (0.01 3)
  (0.025 4)
  (0.1 7)
  (0.05 5)
  (0.25 11)
  (0.5 15)
  (0.75 18)
  (0.84 19)
  (0.9 19)
  (0.95 20)
  (0.975 20)
  (0.99 20)
  (0.999 20)

  var : v-sqrt
  mean: 13.832900000000004
  Percentiles:
  (0.01 3)
  (0.025 4)
  (0.1 7)
  (0.05 5)
  (0.25 11)
  (0.5 15)
  (0.75 18)
  (0.84 19)
  (0.9 19)
  (0.95 20)
  (0.975 20)
  (0.99 20)
  (0.999 20)


|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 20)
   
   (define x (add1 (random-integer n)))
   (define y (add1 (random-integer n)))
   (define z (add1 (random-integer (* n n))))

   (define v-max (max x y))
   (define v-sqrt (inexact->exact (ceiling (sqrt z))))

   (list ; x
         ; y
         v-max
         v-sqrt
         )

   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  ; "x"
                       ; "y"
                       "v-max"
                       "v-sqrt"
                       )
                #:num-samples 30000
                ; #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                #:show-percentiles? #t
                )


