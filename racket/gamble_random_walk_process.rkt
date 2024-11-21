#| 

  Random Walk Process in Racket/Gamble 

  From Mathematica RandomWalkProcess
  """
  RandomWalkProcess[p]
  represents a random walk on a line with the probability of a positive unit step 
  p and the probability of a negative unit step 1-p.
  """

  (random_walk_process_pdf p t x)
  (random_walk_process_cdf p t x)
  (random_walk_process_quantile p t q)
  (random_walk_process_dist p t)
  (random_walk_process_mean p t)
  (random_walk_process_variant p t)

  * t: time slice to study
  * x: the value for pdf/cdf
  * q: quantile

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(newline)
(flush-output)

#|
  Adapted example from Mathematica's RandomWalkProcess:
  """
  A particle starts at the origin and moves to the right by one unit with 
  probability 3/5 and to the left by one unit with probability 2/5 after 
  each second. Find the probability that it has moved to the right by 
  four units after 10 seconds.

  particleMotion = RandomWalkProcess[3/5];
  Probability[x[10] == 4, x in particleMotion] 
  N@%
  -> 
  419904/1953125
  0.214991

  PDF[RandomWalkProcess[3/5][10], 4]
  % // N
  -> 
  419904/1953125
  0.214991
  """

  (random_walk_process_pdf 3/5 10 4) 419904/1953125 0.214990848

  variable : val-t
  2: 489888/1953125 (0.250822656)
  4: 419904/1953125 (0.214990848)
  0: 1959552/9765625 (0.2006581248)
  6: 236196/1953125 (0.120932352)
  -2: 217728/1953125 (0.111476736)
  -4: 82944/1953125 (0.042467328)
  8: 78732/1953125 (0.040310784)
  -6: 20736/1953125 (0.010616832)
  10: 59049/9765625 (0.0060466176)
  -8: 3072/1953125 (0.001572864)
  -10: 1024/9765625 (0.0001048576)
  mean: 2 (2.0)
  HPD interval (0.84): -2..6
  HPD interval (0.9): -4..6
  HPD interval (0.99): -6..8
  HPD interval (0.99999999): -8..10

  variable : prob
  #f: 1533221/1953125 (0.785009152)
  #t: 419904/1953125 (0.214990848)
  mean: 419904/1953125 (0.214990848)


|#

(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define p 3/5)
   (define t 10) ; time slice to study

   (define x (accum (for/list ([i t]) (if (flip p) 1 -1))))

   ; Value at time = t
   (define val-t (last x))
   
   (define prob (= val-t 4)) 

   (list val-t
         prob
         )
   
   )
)

(show2 "(random_walk_process_pdf 3/5 10 4)" (random_walk_process_pdf 3/5 10 4) (* 1.0 (random_walk_process_pdf 3/5 10 4) ))
(newline)
(show-marginals (model)
                (list  "val-t"
                       "prob"
                       "x"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.9 0.99 0.99999999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


#|
  Using random_walk_process_dist 

  variable : d
  2: 0.24991
  4: 0.21552
  0: 0.20032
  6: 0.11988
  -2: 0.11224
  8: 0.04245
  -4: 0.04187
  -6: 0.01004
  10: 0.00618
  -8: 0.00152
  -10: 7e-5
  mean: 2.0175199999999998
  HPD interval (0.84): -2..6
  HPD interval (0.9): -4..6
  HPD interval (0.99): -6..8
  HPD interval (0.99999999): -10..10

  variable : prob
  #f: 0.78448
  #t: 0.21552
  mean: 0.21552


|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define p 3/5)
   (define t 10) ; time slice to study

   (define d (random_walk_process_dist p t))

   (define prob (= d 4)) 

   (list d
         prob
         )
   
   )
)

(displayln "\nModel 2")

(show-marginals (model2)
                (list  "d"
                       "prob"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.9 0.99 0.99999999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

