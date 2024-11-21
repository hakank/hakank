#| 

  Logistic distribution (recover parameters) in Racket/Gamble 

  (min: -9.837200282689777 mean: 3.630986782630146 max: 16.516396152239267)
  variable : mu
  mean: 3.6729009041488014
  HPD interval (0.84): 2.224075715664716..4.873691828392424

  variable : s
  mean: 0.46570799873475427
  HPD interval (0.84): 0.0009425334157531501..0.7780020339005682

  variable : post
  mean: 3.635781724053042
  HPD interval (0.84): 1.743812202711742..4.998390561308898


  This is a port of my WebPPL model logistic_recover.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; Generate some data
(define data (for/list ([i 100]) (logistic_dist 3 2)))

(show2 "min:" (apply min data) "mean:" (avg data) "max:" (apply max data))


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define mu (uniform 0 20))
   (define s (uniform 0 10))
   
   (for ([i (length data)])
     (observe-sample (normal-dist (logistic_dist mu s) 1) (list-ref data i)))

   (define post (logistic_dist mu s))
    
   (list mu
         s
         post
         )
   )
)

(show-marginals (model)
                (list  "mu"
                       "s"
                       "post"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


