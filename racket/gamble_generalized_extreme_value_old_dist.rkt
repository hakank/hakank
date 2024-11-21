#| 

  Generalized extreme value dist in Racket/Gamble 

  From Handbook on probability distributions,
  page 113ff
  """
  xi the shape parameter, mu the location parameter and sigma > 0 the scale parameter.

  The quantile function of the generalized extreme value distribution 
  is F^-1(u) = mu + sigma/xi*((-log u)^-xi)-1 
  for xi != 0. So we can use the inverse function method.
  """


  var : g
  0.8140177429653341: 0.00010000000000000938
  0.6691867485864786: 0.00010000000000000938
  1.4162774411392345: 0.00010000000000000938
  0.5712567676543208: 0.00010000000000000938
  7.5003219401782975: 0.00010000000000000938
  ...
  0.25483399920925764: 0.00010000000000000938
  0.5412560903850103: 0.00010000000000000938
  0.27068394381852245: 0.00010000000000000938
  2.2753916863878283: 0.00010000000000000938
  1.0796227616377525: 0.00010000000000000938
  mean: 8.382505874751088


  Note: This is the old definition and has been replaced with 
  generalized_extreme_value_dist.
 
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

   (define xi 1)
   (define mu 1)
   (define sigma 1)

   (define g (generalized_extreme_value_dist_old xi mu sigma))

   (list g
         )
   )
)

(show-marginals (model)
              (list  "g"
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


(map first (vector->list (generate-samples (model) 10)))


#|
  Recover parameters. 

  importance-sampler is very very slow on this

  variable : mu
  142.2489992291037: 0.34939999999999993
  142.33632306743957: 0.2596999999999999
  142.22720649772202: 0.18079999999999993
  143.06901405100288: 0.18069999999999992
  148.23262158367726: 0.016599999999999997
  141.9953335172171: 0.006799999999999999
  ...
  142.8883056229355: 0.005999999999999998
  mean: 142.51735282501093
  HPD interval (0.01): 141.1539214925553..141.1539214925553
  HPD interval (0.84): 141.1539214925553..141.51702931239288
  HPD interval (0.99): 141.1539214925553..141.9953335172171
  HPD interval (0.9999): 141.1539214925553..141.9953335172171

  variable : sigma
  1.5533400490169251: 0.47739999999999994
  1.4315804831610857: 0.3169999999999999
  1.1762491852650026: 0.09709999999999998
  0.8000491816574313: 0.04509999999999999
  1.1877680532764074: 0.04469999999999999
  1.6927587036261826: 0.018699999999999994
  ...
  mean: 1.4304193862839907
  HPD interval (0.01): 1.6927587036261826..1.6927587036261826
  HPD interval (0.84): 1.7697134569986723..1.801141759994786
  HPD interval (0.99): 1.6927587036261826..1.801141759994786
  HPD interval (0.9999): 1.6927587036261826..1.801141759994786

  variable : xi
  0.2997381645034855: 0.23809999999999998
  0.26512205021115637: 0.23529999999999993
  0.23468483561986264: 0.2192999999999999
  0.3254943622981262: 0.17609999999999992
  0.16504603408499974: 0.11369999999999998
  0.17097734287224883: 0.010499999999999997
  ...
  0.002987668174653077: 0.006999999999999998
  mean: 0.26311872688794646
  HPD interval (0.01): 0.3254943622981262..0.3254943622981262
  HPD interval (0.84): 0.3254943622981262..0.3254943622981262
  HPD interval (0.99): 0.3254943622981262..0.34979868941896775
  HPD interval (0.9999): 0.3254943622981262..0.34979868941896775

  variable : post
  153.66882658124618: 0.03429999999999999
  154.0196645804877: 0.028599999999999993
  149.32805224092124: 0.028599999999999993
  147.2396569959838: 0.028599999999999993
  147.66429787225684: 0.027999999999999994
  ...
  146.97259928334938: 0.0002999999999999999
  147.2009969846447: 0.0002999999999999999
  148.24919015549455: 0.00019999999999999996
  146.2736783118989: 9.999999999999998e-5
  146.2630301801789: 9.999999999999998e-5
  mean: 151.41587623988084
  HPD interval (0.01): 143.82363440224142..143.82363440224142
  HPD interval (0.84): 143.82363440224142..150.50592097859467
  HPD interval (0.99): 143.82363440224142..155.80667144577828
  HPD interval (0.9999): 143.82363440224142..159.10510944890484

  variable : p
  #f: 0.9803000000000001
  #t: 0.019699999999999995
  mean: 0.019699999999999995

|#

(define data (for/list ([i 100])
               (max-list (for/list ([i 1000])
                           (normal 100 15)))))
(show "data" data)
(show2 "min" (min-list data) "mean" (avg data) "max" (max-list data) "stddev" (stddev data) "variance" (variance data))
(newline)
(show-histogram data)
(flush-output)
(newline)

(define (model2)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler ; very very slow
   mh-sampler ; #:transition (slice)

   (define mu (normal (avg data) (stddev data)))
   (define sigma (uniform 0 3))
   ; (define xi (uniform -3 3))
   (define xi (uniform 0 0.99)) ; we know it's maximal values

   (for ([i (range (length data))])
     ; (show "i" i)
     (observe-sample (normal-dist (generalized_extreme_value_dist_old xi mu sigma) 0.10) (list-ref data i))
     ; (observe/fail (<= (abs (- (generalized_extreme_value_dist xi mu sigma) (list-ref data i))) 10))
     )
   
   (define post (generalized_extreme_value_dist_old xi mu sigma))
   (define p (>= post (max-list data)))

   (show2 "mu" mu "sigma" sigma "xi" xi "post" post "p" p)
   
   (list mu
         sigma
         xi
         post
         p
         )
   )
)

(show-marginals (model2)
                (list  "mu"
                       "sigma"
                       "xi"
                       "post"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.01 0.84 0.99 0.9999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 1000
                ; #:thin 10
                )
