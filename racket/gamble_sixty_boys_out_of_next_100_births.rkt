#| 

  Sixty boys out of next 100 births in Racket/Gamble 

  From http://www.statistics101.net/statistics101web_000007.htm
  """
  From CliffsQuickReview Statistics, p. 60, example 4 
  Assuming an equal chance of a new baby being a  
  boy or a girl (that is pi=0.5), what is the 
  likelihood that 60 or more out of the next 100 
  births at a local hospital will be boys? 
  The answer computed from the cumulative 
  binomial distribution is 0.02844. The book's answer, 
  0.0228, is based on the normal approximation to the  
  binomial, and is therefore somewhat in error. 
  """

  Three methods:
  - resampling
  - binomial
  - gaussian approximation

  Using CDF:
  (- 1 (binomial_cdf 100 0.5 59)): 0.02844396682049033

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

#|
  Resampling

  variable : s
  50: 0.07930000000000001
  51: 0.07800000000000001
  52: 0.07470000000000002
  48: 0.07410000000000001
  49: 0.07260000000000001
  ...
  69: 0.00020000000000000006
  31: 0.00020000000000000006
  68: 0.00010000000000000003
  70: 0.00010000000000000003
  30: 0.00010000000000000003
  mean: 50.0515

  variable : p
  #f: 0.969
  #t: 0.031
  mean: 0.031


|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define sample (resample 100 '(0 1))) ; 0: girl, 1: boy
   (define s (sum sample))
   (define p (>= s 60))
   (list s
         p
         )
   )
)

(displayln "Model 1: Resampling")
(show-marginals (model)
                (list  "s"
                       "p"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )



#|
  Bernoulli (exact)

  variable : s
  50: 0.07958923738717873
  49: 0.07802866410507718
  51: 0.07802866410507718
  48: 0.07352701040670735
  52: 0.07352701040670735
  ...
  98: 3.904861480843985e-27
  1: 7.888609052210052e-29
  99: 7.888609052210052e-29
  0: 7.88860905221009e-31
  100: 7.88860905221009e-31
  mean: 50.00000000000001

  variable : p
  #f: 0.9715560331795091
  #t: 0.028443966820490385
  mean: 0.028443966820490385

|#
(define (model2)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define s (binomial 100 0.5))
   (define p (>= s 60))
   (list s
         p
    )    
   )
)

(displayln "Model 2: Bernoulli")
(show-marginals (model2)
                (list  "s"
                       "p"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


#|
  Using Gaussian

  variable : s
  51.966494517226785: 0.00010000000000000938
  44.447792695471925: 0.00010000000000000938
  45.06539502371247: 0.00010000000000000938
  51.566959133177676: 0.00010000000000000938
  53.59510188493085: 0.00010000000000000938
  ...
  51.007336664174964: 0.00010000000000000938
  52.09438684463049: 0.00010000000000000938
  59.46834539095706: 0.00010000000000000938
  43.02310420162158: 0.00010000000000000938
  57.81020504030954: 0.00010000000000000938
  mean: 49.98458143144234

  variable : p
  #f: 0.9763999999999489
  #t: 0.023600000000002296
  mean: 0.023600000000002296

|#
(define (model3)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define mu (* 100 0.5))
   (define sigma (sqrt (* 100 0.5 (- 1 0.5))))
   (define s (normal mu sigma))
   (define p (> s 60))
   (list s
         p
    )    
   )
)

(displayln "Model 3: Gaussian")
(show-marginals (model3)
                (list  "s"
                       "p"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )

(newline)
; (- 1 (binomial_cdf 100 0.5 59)): 0.02844396682049033
(displayln "Using CDF:")
(displayln (format "(- 1 (binomial_cdf 100 0.5 59)): ~a" (- 1 (binomial_dist_cdf 100 0.5 59))))
