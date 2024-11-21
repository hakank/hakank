#| 

  Pareto dist (type I) in Racket/Gamble 

  https://en.wikipedia.org/wiki/Pareto_distribution
  Mathematica: ParetoDistribution[k, alpha]

  variable : v1
  6984.03361198922: 0.00010000000000000938
  9508.605463939464: 0.00010000000000000938
  6870.161330692131: 0.00010000000000000938
  8433.70021743715: 0.00010000000000000938
  6972.333133830145: 0.00010000000000000938
  ...
  7083.191554371845: 0.00010000000000000938
  9861.430280672952: 0.00010000000000000938
  8327.036040715715: 0.00010000000000000938
  7005.333932255736: 0.00010000000000000938
  8180.062177874688: 0.00010000000000000938
  mean: 9066.521413589991
  HPD interval (0.84): 6820.072077312287..10730.097021974007
  Percentiles:
  (0.01 6837.773512037969)
  (0.025 6864.487530067541)
  (0.1 6995.137744211307)
  (0.05 6906.402574960991)
  (0.25 7325.9500939239515)
  (0.5 8121.5886430799455)
  (0.75 9626.1138954275)
  (0.84 10729.237767433606)
  (0.9 12096.83687946251)
  (0.95 14375.298452866673)
  (0.975 16958.218161219324)
  (0.99 21065.7486346926)
  (0.999 42316.12062001199)

  variable : prob1
  #t: 0.8998999999999573
  #f: 0.10010000000000718
  mean: 0.8998999999999573

  variable : v2
  7207.791991404381: 0.00010000000000000938
  9345.044879400528: 0.00010000000000000938
  10743.765837040706: 0.00010000000000000938
  8234.27130972387: 0.00010000000000000938
  8500.782370208908: 0.00010000000000000938
  ...
 8114.788073404168: 0.00010000000000000938
  6916.467383567749: 0.00010000000000000938
  7164.445782325818: 0.00010000000000000938
  9248.917268371984: 0.00010000000000000938
  8359.635767288411: 0.00010000000000000938
  mean: 9081.07042613121
  HPD interval (0.84): 6820.00856598078..10694.54670693789
  Percentiles:
  (0.01 6836.922967892262)
  (0.025 6863.36200423936)
  (0.1 7002.055809878597)
  (0.05 6910.536873819598)
  (0.25 7326.85777493066)
  (0.5 8123.718742124662)
  (0.75 9626.02143385812)
  (0.84 10692.058030193219)
  (0.9 12000.727723024344)
  (0.95 14199.213205498167)
  (0.975 16776.128214062333)
  (0.99 21331.30770593514)
  (0.999 32249.507460609566)
 
  variable : prob2
  #t: 0.9014999999999571
  #f: 0.09850000000000714
  mean: 0.9014999999999571

  Gamble's built-in pareto-dist has parameters (pareto scale shape)
  My pareto_dist has parameters (pareto_dist x alpha) according to the Wikipedia page (op.cit).
  Mathematica's type I: ParetoDistribution[k, alpha].

  Note: It seems that Gamble's quantile function
    (dist-inv-cdf (pareto-dist scale shape) val)
  actually gives 
    (- 1 (dist-inv-cdf (pareto-dist scale shape) val))
  E.g. that the quantile for 0.9 is _less_ than for 0.1
   > (dist-inv-cdf (pareto-dist 6820 4) 0.9)
   7002.026455267925
   > (dist-inv-cdf (pareto-dist 6820 4) 0.1)
   12127.865576465454
  

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

   (define x 6820)   
   (define alpha 4)
   (define v1 (pareto_dist x alpha))
   (define v2 (pareto x alpha)) ; Gamble's built-in (shape scale)

   (define prob1 (>= v1 7000))
   (define prob2 (>= v2 7000))   
    
   (list v1
         prob1
         v2
         prob2
         )
    
   )
)

(show-marginals (model)
                (list  "v1"
                       "prob1"
                       "v2"
                       "prob2"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

#|

  Some different tests
  Using x:6820 alpha:4
  gen: (12046.724736493406 7786.568200419597 7874.53594883506 9972.962866128824 26499.41114714005 7696.11292053839 6959.545676578321 9744.124396934945 14225.722567717268 7971.542303071429)
  (mean 27280/3 9093.333333333334)
  (pdf 7000 13521270961/26260937500000 0.0005148815026500863)
  (cdf <= 7000 1484979039/15006250000 0.09895737036234901)
  (cdf > 7000 13521270961/15006250000 0.901042629637651)
  quantile 0.1 : 7002.026455267925
  quantile 0.5 : 8110.392524318558
  quantile 0.9 : 12127.865576465454
  quantile 0.99 : 21566.733642348343
  quantile 0.99999999 : 681999.9991432786
  quantile 1 : +inf.0

  The built-in quantile for pareto-dist is actually 1 - quantile
  built-in quantile 0.1 : 12127.865576465454
  built-in quantile 0.5 : 8110.392524318558
  built-in quantile 0.9 : 7002.026455267925

  Pareto principle 80-20 law when alpha = log4(5) ~ (log 5 4) ~1.160964047443681. Testing with x=100 
  (i quantile 0 : 100)
  (i quantile 0.1 : 109.49980785090851)
  (i quantile 0.2 : 121.19194114973943)
  (i quantile 0.30000000000000004 : 135.96442360850557)
  (i quantile 0.4 : 155.27091482767867)
  (i quantile 0.5 : 181.67414493210313)
  (i quantile 0.6 : 220.17442281040678)
  (i quantile 0.7 : 282.08710684143927)
  (i quantile 0.7999999999999999 : 399.9999999999999)
  (i quantile 0.8999999999999999 : 726.6965797284121)
  (i quantile 0.9999999999999999 : 5527587364388544.0)

|#
(displayln "\nSome different tests")
(let ([x 6820]
      [alpha 4])
  (displayln (format "Using x:~a alpha:~a" x alpha))
  (show "gen" (repeat (lambda () (pareto_dist x alpha)) 10))
  (show2 "mean" (pareto_dist_mean x alpha) (* 1.0 (pareto_dist_mean x alpha)))
  (show2 "pdf 7000" (pareto_dist_pdf x alpha 7000) (* 1.0 (pareto_dist_pdf x alpha 7000)))
  (show2 "cdf <= 7000" (pareto_dist_cdf x alpha 7000) (* 1.0 (pareto_dist_cdf x alpha 7000)))
  (show2 "cdf > 7000" (- 1 (pareto_dist_cdf x alpha 7000)) (* 1.0 (- 1 (pareto_dist_cdf x alpha 7000))))
  (show "quantile 0.1 " (pareto_dist_quantile x alpha 0.1))        
  (show "quantile 0.5 " (pareto_dist_quantile x alpha 0.5))      
  (show "quantile 0.9 " (pareto_dist_quantile x alpha 0.9))
  (show "quantile 0.99 " (pareto_dist_quantile x alpha 0.99))  
  (show "quantile 0.99999999 " (pareto_dist_quantile x alpha 0.99999999))
  (show "quantile 1 " (pareto_dist_quantile x alpha 1))          
  (newline)
  (displayln "The built-in quantile for pareto-dist is actually 1 - quantile")
  (show "built-in quantile 0.1 " (dist-inv-cdf (pareto-dist x alpha) 0.1))        
  (show "built-in quantile 0.5 " (dist-inv-cdf (pareto-dist x alpha) 0.5))
  (show "built-in quantile 0.9 " (dist-inv-cdf (pareto-dist x alpha) 0.9))
  ; (show "built-in quantile 0 " (dist-inv-cdf (pareto-dist x alpha) 0))  
  (newline)
  (displayln "Pareto principle 80-20 law when alpha = log4(5) ~ (log 5 4) ~1.160964047443681. Testing with x=100 ")
  (for ([i (range 0 1 0.1)])
    (show2 "i quantile" i ":" (pareto_dist_quantile 100 (log 5 4) i))
    )
 
  )
(newline)
