#| 

  Logistic distribution in Racket/Gamble 

  From 
  https://en.wikipedia.org/wiki/Logistic_distribution
  """
  Quantile function
  The inverse cumulative distribution function (quantile function) of the 
  logistic distribution is a generalization of the logit function. Its derivative is 
  called the quantile density function. They are defined as follows:
    Q(p;mu,s) = mu + s * ln(p/(1-p))
    Q(p;s)    = s/(p*(1-p))
  """

  variable : d01
  mean: 0.00974440524597669

  variable : d
  mean: 3.0240983425846095
 
  logistic01_dist mean: 0
  logistic_dist mean 3 1: 3

  See gamble_distributions.rkt and gamble_distributions_test.rkt for more.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

    (define mu 3)
    (define s 1)

    (define d01 (logistic01_dist))    
    (define d (logistic_dist mu s))
    (list d01
          d
    )
   )
)

(show-marginals (model)
                (list  "d01"
                       "d"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )

(displayln (format "logistic01_dist mean: ~a" (logistic01_dist_mean)))
(displayln (format "logistic_dist mean 3 1: ~a" (logistic_dist_mean 3 1)))
(newline)
