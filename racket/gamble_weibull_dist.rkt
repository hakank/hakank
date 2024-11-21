#| 

  Weibull dist in Racket/Gamble 

  From Handbook on probability distributions
  page 69ff

  https://en.wikipedia.org/wiki/Weibull_distribution

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

  
(let ([lambda_ 1000]
      [k 2]
      [x 100])
  (show "generate" (repeat (lambda () (weibull lambda_ k)) 10))
  (show "pdf" (weibull_pdf lambda_ k x))
  (show "cdf" (weibull_cdf lambda_ k x))
  (show "quantile" (weibull_quantile lambda_ k 0.99999))
  (show "mean" (weibull_mean lambda_ k))
  )

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define lambda_ 1000)
   (define k 2)
   
   (define d (weibull lambda_ k))
   (define cdf (<= d 1000))   

   (define max10 (max-list (for/list ([i 10]) (weibull lambda_ k))))
   
   (list d
         cdf
         max10
         )
   
   )
)

(show-marginals (model)
                (list  "d"
                       "cdf"
                       "max10"
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


