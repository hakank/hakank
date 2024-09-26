#| 

  Seven scientists in Racket.Gamble 

  From https://mhtess.github.io/bdappl/chapters/03-simpleModels.html
  """
  Seven scientists with wildly-differing experimental skills all make a 
  measurement of the same quantity. They get the answers 

     x = (−27.020, 3.570, 8.191, 9.898, 9.603, 9.945, 10.056). 

  Intuitively, it seems clear that the first two scientists are pretty inept 
  measurers, and that the true value of the quantity is probably just a bit 
  below 10. The main problem is to find the posterior distribution over 
  the measured quantity, telling us what we can infer from the measurement. 
  A secondary problem is to infer something about the measurement skills of 
  the seven scientists.
  """

  var : mu
  mean: 9.714848866443942

  var : sigmas 0
  mean: 16.930925175133105

  var : sigmas 1
  mean: 5.55582301053429

  var : sigmas 2
  mean: 7.337444934020477

  var : sigmas 3
  mean: 4.586791146514205

  var : sigmas 4
  mean: 7.715405905682412

  var : sigmas 5
  mean: 1.1500528770141196

  var : sigmas 6
  mean: 2.0673929339384447

  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define data '(-27.020 3.570 8.191 9.898 9.603 9.945 10.056))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler 

   (define mu (normal 0 30))
   (define sigmas (repeat (lambda () (uniform 0 20)) 7))
   ; (defmem (sigmas i) (uniform 0 30))

   (map (lambda (i d)
          ; (observe-sample (normal-dist mu (sigmas i)) d)
          (observe-sample (normal-dist mu (list-ref sigmas i)) d)          
          )
        (range (length data))
        data)
   
   (list mu
         ;; (sigmas 0)
         ;; (sigmas 1)
         ;; (sigmas 2)
         ;; (sigmas 3)
         ;; (sigmas 4)
         ;; (sigmas 5)
         ;; (sigmas 6)

         (list-ref sigmas 0)
         (list-ref sigmas 1)
         (list-ref sigmas 2)
         (list-ref sigmas 3)
         (list-ref sigmas 4)
         (list-ref sigmas 5)
         (list-ref sigmas 6)
         
         )
   )
)

(show-marginals (model)
                (list  "mu"
                       "sigmas 0"
                       "sigmas 1"
                       "sigmas 2"
                       "sigmas 3"
                       "sigmas 4"
                       "sigmas 5"
                       "sigmas 6"
                       )
                #:num-samples 20000
                ; #:truncate-output 2
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


