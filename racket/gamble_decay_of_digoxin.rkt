#| 

  Decay of digoxin in Racket Gamble.

   From A First Course in Mathematical Modeling, 4th edition,
  page 14f:
  """
  Digoxin is used in the treatment of heart disease. Doctors must prescribe an amount of
  medicine that keeps the concentration of digoxin in the bloodstream above an effective level
  without exceeding a safe level (there is variation among patients). For an initial dosage of
  0.5 mg in the bloodstream, Table 1.2 shows the amount of digoxin a n remain-
  ing in the bloodstream of a particular patient after n days, together with the change Δa n
  each day.
  """
  
  The book states the formula:

     a[n] = a[n] - 0.31*a[n] = 0.69*a[n-1]
     a[1] = 0.5

  Here we tests with and without intercept

  with-intercept: #f
  var : slope
  mean: 0.6830145814962331

  var : v
  mean: 0.009522722031360002

  with-intercept: #t
  var : slope
  mean: 0.7327367354430016

  var : intercept
  mean: -0.0019910274473137956

  var : v
  mean: 0.029376156597217127


  This is a port of my WebPPL model decay_of_digoxin.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model with-intercept)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

    (define a '(0.500  0.345  0.238  0.164  0.113  0.078  0.054  0.037  0.026))

    (define n (length a))
    
    (define slope (uniform -1 1))
    (define v (sample (clip-distx (normal-dist 0 1) 0 10000)))

    (define intercept (normal 2 1))
      
    (for ([i (range 1 n)])
      (if with-intercept
          (observe-sample (normal-dist (+ intercept (* (list-ref a (- i 1)) slope)) v) (list-ref a i))
          (observe-sample (normal-dist (+ 0         (* (list-ref a (- i 1)) slope)) v) (list-ref a i))
          
          ))

    (if with-intercept
        (list slope
              intercept
              v)
        (list slope
              v)
        )
        
  
   )
)

(for ([with-intercept '(#f #t)])
  (show "with-intercept" with-intercept)
  (show-marginals (model with-intercept)
                  (if with-intercept
                      (list  "slope"
                             "intercept"
                             "v" 
                             )
                      (list  "slope"
                             "v" 
                             )
                      
                      )
                  #:num-samples 10000
                  #:truncate-output 5
                  #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )
)
