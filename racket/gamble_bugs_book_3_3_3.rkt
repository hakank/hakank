#| 

  BUGS Book, Example 3.3.3 in Racket.Gamble 

  Example 3.3.3 Trihalomethanes in tap water
  Cf ~/jags/bugs_book_3_3_3.R
  """
  model (
    for (i in 1:n) (
      y(i) ~ dnorm(mu, inv.sigma.squared)
    )
    mu ~ dnorm(gamma, inv.omega.squared)
    inv.omega.squared <- n0/sigma.squared
    inv.sigma.squared <- 1/sigma.squared
    y.pred ~ dnorm(mu, inv.sigma.squared)
    P.crit <- step(y.pred - y.crit)
  )

  Data: list(n=2, y=c(128, 132), gamma=120, n0=0.25,sigma.squared=25, y.crit=145)

  Output:
            Mean      SD  Naive SE Time-series SE
P.crit 3.213e-03 0.05659 0.0002001      0.0002034
mu     1.289e+02 3.33096 0.0117767      0.0117332
y.pred 1.289e+02 5.95716 0.0210618      0.0209726 
  """

  var : p_crit
  mean: 0.0051000000000001955

  var : mu
  mean: 129.7444135580946

  var : y_pred
  mean: 129.1213094917327


  This is a port of my WebPPL model bugs_book_3_3_3.wppl.
  
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
   ; importance-sampler
   mh-sampler

   (define n 2)
   ;; fixed Real() y =(128.0, 132.0); ;; Moved to obs
   (define gamma 120)
   (define n0 0.25)
   (define sigma_squared 25)
   (define y_crit 145.0)
    
   (define omega_squared (/ sigma_squared n0))
   (define mu (normal gamma omega_squared))
   (define (y i) 
     (normal-dist mu (sqrt sigma_squared)))
   
   ;; random Real inv_sigma_squared ~ 1/sigma_squared;
   (define y_pred (normal mu (sqrt sigma_squared)))
   (define p_crit (> y_pred y_crit))

   (observe-sample (y 0) 128)
   (observe-sample (y 1) 132)
  
   (list p_crit
         mu
         y_pred
    )

   )
)

(show-marginals (model)
                (list  "p_crit"
                       "mu"
                       "y_pred"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


