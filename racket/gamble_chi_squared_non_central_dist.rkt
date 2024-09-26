#| 

  Chi squared non central dist in Racket.Gamble 

  From Handbook on probability distributions
  page 78ff


  var : g
  4.4379840213447235: 0.00010000000000000938
  0.2372729127075176: 0.00010000000000000938
  1.2939118542771328: 0.00010000000000000938
  2.78062470950914: 0.00010000000000000938
  3.150217436610659: 0.00010000000000000938
  ...
  0.25968466253559097: 0.00010000000000000938
  3.140095617996523: 0.00010000000000000938
  -1.4200266610732728: 0.00010000000000000938
  -0.3132202901409991: 0.00010000000000000938
  2.3848888857828: 0.00010000000000000938
  mean: 1.552038125559268

  var : g2
  0.7269628780550148: 0.00010000000000000938
  1.2716574820498963: 0.00010000000000000938
  2.04784020240628: 0.00010000000000000938
  3.1022589238629434: 0.00010000000000000938
  5.256329217132665: 0.00010000000000000938
  ...
  2.38538214120744: 0.00010000000000000938
  -1.2231042190890484: 0.00010000000000000938
  1.1551094977424008: 0.00010000000000000938
  -0.4745087956369225: 0.00010000000000000938
  1.1370853345957916: 0.00010000000000000938
  mean: 1.453160661415861

  var : lambda
  1769/3600: 0.9999999999999463
  mean: 0.49138888888886245


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

#|
  From Handbook on probability distributions
  page 79
  """
  For integer k degrees of freedom, we can use the definition
  of the sum, i.e. sum k i(n)dependent normal random variables
  N(sqrt(lambda,k),1).
  """
  Note: I'm not sure how to interpret lambda: Is it the sum of the squares of the mu's
        or the actual parameter to the function?
        Also, how do we interpret the case with just k as a parameter, i.e. without lambda?
|#
(define (non_central_chi_squared_dist_mu k mu)
  (for/sum ([m mu]) (normal m 1)))

;; Here we use the lambda direct:
(define (non_central_chi_squared_dist k lambda_) 
  (for/sum ([i k]) (normal (sqrt (/ lambda_ k)) 1)))

(define (lambda_func mu) 
  (for/sum ([m mu]) (expt m 2)))


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define k 5)
   ;; (define mu '(1/2 1/3))
   (define mu '(1/2 1/3 1/4 1/5 1/6))
   (define lambda_ (lambda_func mu))
   (define g (non_central_chi_squared_dist k lambda_))
   (define g2 (non_central_chi_squared_dist_mu k mu))

   (list g
         g2
         lambda_
         )

   )
)

(show-marginals (model)
                (list  "g"
                       "g2"
                       "lambda"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


