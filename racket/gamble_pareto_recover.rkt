#| 

  Pareto distribution, recover parameters in Racket/Gamble 

  Trying to recover parameters from 100 instances of (pareto 100 2).
  Not very good:


  (min 100.15667450086451 mean 188.13812848447694 max 706.5782164444969 stddev 123.73923851978472)
  pareto_mean 100 2: 200

  variable : x
  99.32103051073953: 0.5549999999999999
  98.79162485259745: 0.44499999999999995
  mean: 99.08544499286629
  HPD interval (0.84): 100.20022296359141..101.80827978244739

  variable : alpha
  3.538418727924837: 0.927
  3.0914073840278986: 0.051
  3.1337013472360282: 0.022
  mean: 3.5067173670109395
  HPD interval (0.84): 1.9290445608182971..2.569986703935367

  variable : post
  100.05173892810778: 0.147
  166.58434205319088: 0.136
  136.4628389987354: 0.136
  119.73744499460943: 0.132
  125.8571447655684: 0.104
  ...
  162.18282972442086: 0.062
  104.1776070313311: 0.053000000000000005
  218.81780183886136: 0.036
  118.92147244218447: 0.018
  146.9758999541364: 0.003
  mean: 130.07651379609428
  HPD interval (0.84): 101.9985834885802..241.29724922940653


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(define data (repeat (lambda () (pareto 100 2)) 100))
(show "data" data)
(show2 "min" (apply min data) "mean" (avg data) "max" (apply max data) "stddev" (stddev data))
(show "pareto_mean 100 2" (pareto_dist_mean 100 2))
(define (model)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler	
   mh-sampler

   (define x (normal (apply min data) 1))
   (define alpha (uniform 0 10))

   (for ([i (length data)])
     (observe-sample (normal-dist (pareto x alpha) 100) (list-ref data i)))

   (define post (pareto x alpha))

   (show2 x alpha post)
   
   (list x alpha post)

   )
)

(show-marginals (model)
                (list  "x"
                       "alpha"
                       "post"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                #:burn 1000
                #:thin 10
                )


