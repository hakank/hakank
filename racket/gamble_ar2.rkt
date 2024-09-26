#| 

  AR(2) in Racket Gamble.

  From BLOG example/ar2.dblog

-0.13417953845622776: 0.0009999999999999898
0.02896330679812241: 0.0009999999999999898
-0.17927676761663514: 0.0009999999999999898
-0.06309108805781145: 0.0009999999999999898
...
0.039210052937553794: 0.0009999999999999898
0.015260048358102372: 0.0009999999999999898
-0.08521180074636646: 0.0009999999999999898
-0.1199072084617343: 0.0009999999999999898
mean: -0.0046867400993639155
Min: -0.4152798725104616 Mean: 0.004001768113005985 Max: 0.37885139485788377 Variance: 0.017822721067799554 Stddev: 0.13350176428721663
Credible interval (0.84): -0.18690240808004066..0.18789957823209164

var : x 3
-0.12963570992960913: 0.0009999999999999898
0.07595758071917295: 0.0009999999999999898
0.16177337721455254: 0.0009999999999999898
-0.051435674801584855: 0.0009999999999999898
...
0.13718265966655835: 0.0009999999999999898
0.07573692102804683: 0.0009999999999999898
-0.15661147806168588: 0.0009999999999999898
-0.11327658391726403: 0.0009999999999999898
mean: -0.002019244826874998
Min: -0.4417610841396613 Mean: -0.005074177309789366 Max: 0.46680861046223804 Variance: 0.021910370027557812 Stddev: 0.14802151879898345
Credible interval (0.84): -0.20046779338586396..0.1995044167644679


  This is a port of my WebPPL model ar2.wppl 
 
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice) 
   
   (define beta '(0.2 0.8))
   (define sigma 0.1)
   
    
   (define (x t) 
     (cond
       [(= t 0) (dist-unit 0)]
       [(= t 1) (normal-dist 0.0 sigma)]
       [else (let ([xt1 (sample (x (- t 1)))]
                   [xt2 (sample (x (- t 2)))])
               (normal-dist (+ (* (list-ref beta 0) xt2)
                               (* (list-ref beta 1) xt1))
                            sigma))]
       ))
   
   (observe-sample (x 1) 1.0)
   
   (list ; (sample (x 0))
         ; (sample (x 1))
         (sample (x 3))
         (sample (x 2))
         )
         

   )
  )

(show-marginals (model)
                (list ; "x 0"
                      ; "x 1"
                      "x 2"
                      "x 3"
                      "sigma"
                        )
                  #:num-samples 1000
                  #:truncate-output 4
                  ; #:skip-marginals? #t
                  #:credible-interval 0.84
                  #:show-stats? #t
                  ; #:show-histogram? #t
                  )

