#| 

  Extreme value distribution (experimental) in Racket.Gamble 

  From https://www.randomservices.org/random/special/ExtremeValue.html
  """
  The distribution is also known as the standard Gumbel distribution in honor of Emil Gumbel. 
  As we will show below in (13), it arises as the limit of the maximum of independent random 
  variables, each with the standard exponential distribution (when this maximum is 
  appropriately centered). This fact is the main reason that the distribution is special, 
  and is the reason for the name. For the remainder of this discussion, suppose that random variable 
  has the standard Gumbel distribution.

  ...

  The quantile function G^-1 of V given by 
     G^-1(p) = -ln(-ln(p))  p in 0..1
   """

  This is implemented as extreme_value_dist1.

  This seems to be about the same as Mathematica's ExtremeValueDistribution(0,1)

  From Mathematica ExtremeValueDistribution
  """
  ExtremeValueDistribution(alpha,beta)
  represents an extreme value distribution with location parameter alpha and scale parameter beta

  --- 

  The extreme value distribution gives the asymptotic distribution of the maximum value 
  in a sample from a distribution such as the normal distribution.

  ...
   
  Quantile(ExtremeValueDistribution(a,b), x)
  -> 
  a-b Log(-Log(x))    0 < x < 1
  Infinity            x <= 0   if 0 <= x <= 1
  Infinity             True
  """

  Generating data is fairly good, but trying to restore the data is not impressive.
  Thus, it should be considered experimental...


  This is a port of my WebPPL model extreme_value_dist.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


#|

Model 1
var : d
mean: 0.5821559604736841
Min: -2.3682779581539233 Mean: 0.5582625999569129 Max: 10.671599982181636 Variance: 1.5888646220070604 Stddev: 1.260501734234055
Credible interval (0.94): -1.4361934023311484..3.0025387700918107

var : p
mean: 0.6320999999999868
Min: 0 Mean: 0.6324 Max: 1 Variance: 0.23247024 Stddev: 0.48215167737963954
Credible interval (0.94): 0..1

|#
[define (model1)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice)
   
   (define d (extreme_value_dist1))
   (define p (> d 0))
   
   (list d
         p
         )
   
   )
  ]

(displayln "Model 1")
(show-marginals (model1)
                (list  "d"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

#|

var : d
mean: 0.5805431239346457
Min: -2.186094048061195 Mean: 0.5574824834527441 Max: 10.445746805084058 Variance: 1.6876245821510472 Stddev: 1.2990860564839601
Credible interval (0.94): -1.570640795697105..2.976841275820208

var : p
mean: 0.6295999999999871
Min: 0 Mean: 0.6196 Max: 1 Variance: 0.23569584 Stddev: 0.48548515940242704
Credible interval (0.94): 0..1

|#
(define (model2)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice)

   (define a 0)
   (define b 1)
   (define d (extreme_value_dist2 a b))
   (define p (> d 0))
    
   (list d
         p
         )

   )
)

(displayln "Model 2")
(show-marginals (model2)
                (list  "d"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

#|

  Testing with larger values

var : d
mean: 26.06400336882799
Min: -6.828769209910281 Mean: 25.7784782412895 Max: 138.2729191416558 Variance: 236.32311736721545 Stddev: 15.372804473069168
Credible interval (0.94): 1.4597489562396824..54.8998095885941

var : p
mean: 0.9917999999999472
Min: 0 Mean: 0.9921 Max: 1 Variance: 0.00783759 Stddev: 0.08853016435091488
Credible interval (0.94): 1..1

|#
(define (model2b)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice)

   (define a 19)
   (define b 12)
   (define d (extreme_value_dist2 a b))
   (define p (> d 0))
    
   (list d
         p
         )

   )
)

(displayln "Model 2b")
(show-marginals (model2b)
                (list  "d"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


#|
   Well, restoring data is not especially good. 

Generate some values for extreme_value_dist2(19,12):
(29.053691784562726 24.497445238492233 13.09308612908194 10.566873222925242 15.27976484075458 26.898530959167907 26.575586733570226 33.57039720423452 31.44186033398831 17.31941421603424 40.79874905909206 26.874761364992953 10.382817996022833 54.45275621096761 25.057839389251523 65.2579057932922 42.550588590005404 22.04172087333451 20.391905624020165 33.85196532183579 14.7635056313029 24.944051459684715 12.941767780573352 38.74944741471097 20.142321168085463 34.51839297265296 53.17980577877118 12.896129831920177 66.30834846942142 25.043466496557052)
length:: 30
(min: 10.382817996022833 mean: 29.11482992964357 max: 66.30834846942142)

Restoring parameters....
Model 3
var : a
mean: 27.430376959920064
Min: 22.175725761929286 Mean: 26.480709661404205 Max: 30.429839279830123 Variance: 3.696743457580737 Stddev: 1.9226917219306732
Credible interval (0.94): 23.290355304347795..29.77394133177116
Percentiles::
(0.01 23.122187147246425)
(0.1 23.657549224041926)
(0.025 23.122187147246425)
(0.25 24.90769167914984)
(0.5 26.436770031891804)
(0.75 28.11765057232029)
(0.84 28.269156040620167)
(0.9 29.13156344074877)
(0.975 29.85597702442744)
(0.99 30.429839279830123)
(0.999 30.429839279830123)

var : b
mean: 6.970801698839477
Min: 9.401340516162763 Mean: 14.031316758500031 Max: 23.160622715346875 Variance: 14.28762048841188 Stddev: 3.779896888595227
Credible interval (0.94): 10.399630517494668..23.160622715346875
Percentiles::
(0.01 9.401340516162763)
(0.1 10.399630517494668)
(0.025 9.85511037285043)
(0.25 10.926691250119308)
(0.5 13.122206793497087)
(0.75 14.979962728878544)
(0.84 17.271859243639447)
(0.9 18.709201573281067)
(0.975 23.160622715346875)
(0.99 23.160622715346875)
(0.999 23.160622715346875)

var : post
mean: 30.638049265474695
Min: -16.911253545286392 Mean: 34.7355987804664 Max: 121.89825009774808 Variance: 319.30519390768546 Stddev: 17.869112846128804
Credible interval (0.94): 2.4469342133532876..70.24860032204391
Percentiles::
(0.01 2.738766403122579)
(0.1 15.863700532422621)
(0.025 7.131552904850718)
(0.25 22.59328956440187)
(0.5 32.328822381717046)
(0.75 43.56684862464738)
(0.84 49.035325467412235)
(0.9 54.868725846617856)
(0.975 78.89649657326268)
(0.99 86.97618347048495)
(0.999 92.9963165329323)


Mathematica give the following estimation of the data:
ExtremeValueDistribution[22.428, 10.9849]

|#

; Generate random values and then try to restore the data
(displayln "Generate some values for extreme_value_dist2(19,12):")
(define data (for/list ([i 30]) (extreme_value_dist2 19 12)))
(displayln data)
(show "length:" (length data))
(show2 "min:" (apply min data) "mean:" (avg data) "max:" (apply max data))

(displayln "\nRestoring parameters....")


(define (model3)
  (; enumerate 
   ; rejection-sampler
   ; importance-sampler ; Generates just a single value
   mh-sampler ; #:transition (slice)
  
   (define a (uniform 0 100))
   (define b (uniform 0 100))

   (define sigma .1)
   (for ([v data])
     (observe-sample (normal-dist (extreme_value_dist2 a b) sigma) v)
     )
   
   (define post (extreme_value_dist2 a b))
   
   (list a
         b
         post
         )
   
   )
)


(displayln "Model 3")
(show-marginals (model3)
                (list  "a"
                       "b"
                       "post"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                #:show-percentiles? #t
                )
