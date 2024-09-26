#| 

  Negative Binomial distribution in Racket Gamble.

  See gamble_distributions.rkt for details and 
  gamble_distributions_test.rkt for some more tests.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

#|
  var : v
  0: 0.34300342843668685
  1: 0.3087030855930182
  2: 0.18522185135581096
  3: 0.09261092567790549
  4: 0.04167491655505745
  5: 0.01750346495312413
  6: 0.0070013859812496455
  7: 0.0027005345927677217
  8: 0.0010127004722878948
  9: 0.0003713235065055633
  10: 0.00013367646234200225
  11: 4.739438210307371e-5
  12: 1.5312031140993005e-5
  mean: 1.2855939768981777

  var : p
  #f: 0.9295392910634215
  #t: 0.07046070893657849
  mean: 0.07046070893657849

|#
(define (model)
  
  (enumerate ; we need to limit this since geometric (which negative_binomial use) is infinite
   #:limit 1e-05
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define v (negative_binomial 3 0.7))   
   (define p (>= v 4))
   
   (list v
         p
         )
         
   )
  )

#|
   Model 2
  var : v
  6: 0.6200190934880546
  7: 0.23915022177396406
  8: 0.08968133316523644
  9: 0.03288315549392019
  10: 0.01183793597781122
  11: 0.004197086392133082
  12: 0.0014689802372465771
  13: 0.0005084931590468944
  14: 0.00017434051167321987
  15: 5.9275773968895115e-5
  16: 2.000557371450204e-5
  17: 7.84532302529492e-8
  mean: 6.6000019887766985

  var : p
  #f: 0.9817338039211753
  #t: 0.018266196078824643
  mean: 0.018266196078824643

|#
(displayln "\nModel 1")
(show-marginals (model)
                (list "v"
                      "p"
                      )
                #:num-samples 1000
                ; #:truncate-output 1
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                )


(define (model2)
  
  (enumerate ; we need to limit this since geometric (which negative_binomial use) is infinite
   #:limit 1e-05
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ; (define v (negative_binomial_exact 3 0.7))
   (define v (negative_binomial 3 0.7))   

   (observe/fail (> v 5))
   (define p (>= v 10 ))
   
   (list v
         p
         )
         
   )
  )

(displayln "\nModel 2")
(show-marginals (model2)
                (list "v"
                      "p"
                      )
                #:num-samples 1000
                ; #:truncate-output 1
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                )
