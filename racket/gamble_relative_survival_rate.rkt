#| 

  Relative Survival Rate in Racket/Gamble 

  This is from Mathematica's MortalityData, 
  more specificially for me (a 66 yo male in Sweden).

  Here's the Mathematica code for finding out the distribution (via simulation):
  """
  dist = MortalityData(<|"Age" -> Quantity(66, "Years"), 
                                          "Gender" -> "Male", 
                                           "Country" -> "Sweden"|>, 
                                           "DistributionDimensionless")
  data = RandomVariate(dist, 100000);
  FindDistribution(data, 5)

  -> (MixtureDistribution((0.259614, 0.740386),  
     (LogisticDistribution(68.7372, 9.35127), 
     GammaDistribution(127.945, 0.66129))), 

     MixtureDistribution((0.199834, 0.800166), 
     (CauchyDistribution(64.9097, 5.39639), 
     LogNormalDistribution(4.43461, 0.0872523))), 

     MixtureDistribution((0.115708, 0.884292), 
     (CauchyDistribution(60.7114, 6.01232), 
     GammaDistribution(93.0262, 0.895315))), 

     MixtureDistribution((0.0666283, 0.933372), 
     (NormalDistribution(52.2537, 26.762), 
     NormalDistribution(82.1081, 9.65312))), 

     StudentTDistribution(82.1917, 8.91029, 3.59799))
   """

   Let's try the first alternative:
     MixtureDistribution[{0.259614, 0.740386}, 
        {LogisticDistribution[68.7372, 9.35127], 
         GammaDistribution[127.945, 0.66129]
        }]


  Here are some queries. 
  I am 67 yo now, what is the probability that I will be 
  72, 77, 82, 87, and 100 yo?

  * p: surv >= 67 obs: surv >= #f
  variable : p
  mean: 0.974799999999949

  variable : surv
  mean: 80.56882100145229

  * p: surv >= 72 obs: surv >= 67
  variable : p
  mean: 0.9109999999999561

  variable : surv
  mean: 80.83561318388504

  * p: surv >= 77 obs: surv >= 67
  variable : p
  mean: 0.7047999999999788

  variable : surv
  mean: 80.9344044542344
 
  * p: surv >= 82 obs: surv >= 67
  variable : p
  mean: 0.4255000000000095

  variable : surv
  mean: 80.96646696918758

  * p: surv >= 87 obs: surv >= 67
  variable : p
  mean: 0.17890000000001693

  variable : surv
  mean: 80.93883384233604

  * p: surv >= 100 obs: surv >= 67
  variable : p
  mean: 0.0038000000000003565


  * When my mother was 98 years, what was the probability that she would be 100?

  * p: surv >= 100 obs: surv >= 98
  variable : p
  mean: 0.5137999999999998

  variable : surv
  mean: 100.84940646319185

  (Unfortunately, she passed away some weeks before she held her 99'th 
   birthday. RIP!)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model p-dir p-val obs)
  (displayln (format "* p: surv ~a ~a obs: surv >= ~a"
                     (cond 
                       [(eq? p-dir >=) ">="]
                       [(eq? p-dir >) ">"]
                       [(eq? p-dir <) "<"]
                       [(eq? p-dir <=) "<="]
                       [else "??"])
                     p-val obs))
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ; (define log_dist (logistic_dist 68.7372  9.35127))
   (define log_dist (logistic 68.7372  9.35127))   
   (define gamma_dist (gamma 127.945 0.66129))

   (define surv (+ (* 0.259614 log_dist) (* 0.740386 gamma_dist)))

   (define p (p-dir surv p-val))
   
   ; conditioned by
   (when obs
       (observe/fail (>= surv obs))
       )
    
   (list p
         surv         
         ; log_dist
         ; gamma_dist
         )
   )
  )


(for ([p-val '(67 72 77 82 87 100   100)]
       [obs  '(#f 67 67 67 67 67     98)])
  (show-marginals (model >= p-val obs )
                  (list  "p"
                         "surv"
                         "log_dist"
                         "gamma_dist"
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
  )


