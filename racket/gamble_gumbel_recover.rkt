#| 

  Gumbel distribution, recovering parameters in Racket/Gamble 

  Trying to Recover (gumbel_dist 12 3). 
  Not too bad:
  
  (length 100 min: -9.2354645980914 mean: 9.56644455752442 max: 16.45717534294735)
  var : a
  mean: 11.6672224144985
  Min: 6.711143777686615 Mean: 10.151534213438138 Max: 14.462861826241777 Variance: 0.8163665616258938 Stddev: 0.9035300557402027
  HPD interval (0.84): 9.025828352517518..11.261384052775774

  var : b
  mean: 1.739231186650312
  Min: 1.0727847814884117 Mean: 1.4662367180941838 Max: 3.3902711815145814 Variance: 0.1764198720783284 Stddev: 0.4200236565698751
  HPD interval (0.84): 1.0727847814884117..1.6102680137697019

  var : post
  mean: 10.19590495827172
  Min: 5.323091590476009 Mean: 9.746223964694797 Max: 14.400720850675368 Variance: 1.4977103407860337 Stddev: 1.2238097649496158
  HPD interval (0.84): 8.490116655689729..11.048036627983505

  gumbel_dist_mean 12 3 : 10.2683530052954)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define data (for/list ([i 100]) (gumbel_dist 12 3)))

(show2 "length" (length data) "min:" (apply min data) "mean:" (avg data) "max:" (apply max data))


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define a (uniform 1 100))
   (define b (uniform 1 100))

   (define sigma 10)
   (for ([i (length data)])
     (observe-sample (normal-dist (gumbel_dist a b) sigma) (list-ref data i)))
   
   (define post (gumbel_dist a b))
    
   (list a
         b
         post
    )

   )
)

(show-marginals (model)
                (list  "a"
                       "b"
                       "post"
                     )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


(show "(gumbel_dist_mean 12 3)" (gumbel_dist_mean 12 3))
