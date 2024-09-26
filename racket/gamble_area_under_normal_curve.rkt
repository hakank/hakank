#| 

  Area under normal curve in Racket.Gamble 

  From http://www.statistics101.net/statistics101web_000007.htm
  """
  From CliffsQuickReview Statistics, p. 56, example 2 
  A normal distribution of retail store purchases has 
  a mean of $14.31 and a standard deviation of 6.40. 
  What percentage of purchases were under $10? 
  """

  The mh-sampler is quire faster than importance-sampler on this model.

  var : d
  mean: 14.255168154086537

  var : p
  mean: 0.2507000000000535



  The exact probabiity is

  > (dist-cdf (normal-dist 14.31 6.40) 10)
  0.2503344990875074



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

   (define d (normal 14.31 6.40))
   (define p (< d 10))

   (list d
         p
         )
   
   )
)

(show-marginals (model)
                (list  "d"
                       "p"
                       
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


