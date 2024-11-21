#| 

  Minimum daily flows in Racket/Gamble 

  From Mathematica MinStableDistribution
  """
  MinStableDistribution can be used to model the annual minimum mean daily 
  flows. Consider the Mahanadi river and the minimum flows given in cubic meters per second:
  """
   
  Here we use Gumbel distribution (instead of MinStableDistribution) which also
  is for minimum extreme values.

  """
  edist = EstimatedDistribution[minFlow, GumbelDistribution[a, b]]
  --> 
  GumbelDistribution[2.95485, 1.28719]

  Mean[edist]
  -> 2.21187

  Find the probability that the minimum flow is 1.5 cubic meters per second or less:
  NProbability[x <= 1.5, x e edist]
  -> 
  0.275991

  Assuming that the annual minimum flows are independent, find the probability that the 
  minimum flow will not exceed 2 cubic meters per second for 3 consecutive years:

 [Note the cube]  

  NProbability[x <= 2, x e edist]^3
  -> 0.0543934


  NProbability[x <= 2, x e edist]
  -> 0.378892

  """

  mean: 2.3645454545454543
  Min: 0.85 Mean: 2.3645454545454543 Max: 5.32 Variance: 1.1614520661157028 Stddev: 1.0777068553719527
  var : mu
  mean: 2.4954690045909733

  var : sigma
  mean: 1.4172681559183173

  var : post
  mean: 1.6300681026763162

  var : p1
  mean: 0.3770501995534251

  var : p2
  mean: 0.4979213669415738

  Probability for flow <= 2.0 in 1..10 consecutive years:
  p2^1: 0.46617084789227103
  p2^2: 0.2173152594245989
  p2^3: 0.10130603874589411
  p2^4: 0.04722592197878072
  p2^5: 0.022015348091342448
  p2^6: 0.0102629134863846
  p2^7: 0.004784271081792932
  p2^8: 0.0022302877067458837
  p2^9: 0.0010396951112974374
  p2^10: 0.0004846755515829755

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define data '(2.78 2.47 1.64 3.91 1.95 1.61 2.72 3.48 0.85 2.29 1.72 
                    2.41 1.84 2.52 4.45 1.93 5.32 2.55 1.36 1.47 1.02 1.73))

(show "mean" (avg data))
(show-stats data)

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define mu (normal (avg data) (stddev data)))
   (define sigma (uniform 0 10))
   (define sigmaGauss 10)
   (for ([i (length data)])
     (observe-sample (normal-dist (gumbel_dist mu sigma) sigmaGauss) (list-ref data i)))
   
   (define post (gumbel_dist mu sigma))
   (define p1 (<= post 1.5))
   (define p2 (boolean->integer (<= post 2)))
   (list mu
         sigma
         post
         p1
         p2
         ; sigmaGauss
         )
   )
)

(show-marginals (model)
                (list  "mu"
                       "sigma"
                       "post"
                       "p1"
                       "p2"
                     )
                    #:num-samples 10000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )

(displayln "Probability for flow <= 2.0 in 1..10 consecutive years:")
(let ([p2 (match (first (get-probs (model) #:ix 4))
              [(list (list 1 one) (list 0 zero)) one]
              [(list (list 0 zero) (list 1 one)) one]
              )])
  (for ([year (range 1 11)])
    (displayln (format "p2^~a: ~a" year (expt p2 year))))
)
(newline)

(define mu (vals*weights (first (get-probs (model) #:ix 0))))
(define sigma (vals*weights (first (get-probs (model) #:ix 1))))

(show2 "mu" mu "sigma" sigma)
(displayln (format "gumbel_dist_mean ~a ~a: ~a" mu sigma (gumbel_dist_mean mu sigma)))
