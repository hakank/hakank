#| 

  Device life time (Pareto distribution) in Racket Gamble.

  From Mathematica ParetoDistribution
  """
  The lifetime of a device follows ParetoDistribution:
  D = ParetoDistribution(Quantity(1, "Years"), 1.23);

  Find the average lifetime of this device:
  Mean(D)
  -> Quantity(5.34783, "Years")

  Find the probability that the device will be operational for more than 6 years:
  Probability(x > Quantity(6, "Years"),  x -> D)
  -> 0.110376
  """

var : g
20.20223058972051: 0.00010000000000000938
1.321982832369787: 0.00010000000000000938
2.6902320305120435: 0.00010000000000000938
1.5901026671379228: 0.00010000000000000938
...
1.66646641961205: 0.00010000000000000938
21.862276177920087: 0.00010000000000000938
4.15672752483672: 0.00010000000000000938
3.3407513565286413: 0.00010000000000000938
mean: 7.325752954280416
Min: 1.0000871836787961 Mean: 5.163437472779137 Max: 3518.4265166030314 Variance: 2342.412399048012 Stddev: 48.39847517275737
ix: 0
Credible interval (0.93): 1.0000871836787961..8.662226195070929

var : p
#f: 0.8864999999999588
#t: 0.11350000000000757
mean: 0.11350000000000757
Min: 0 Mean: 0.1076 Max: 1 Variance: 0.09602224 Stddev: 0.3098745552639003
ix: 0
Credible interval (0.93): 0..1


  This is a port of my WebPPL pareto_device_life_time.wppl

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
   ; mh-sampler

   (define g (pareto 1 1.23))
   (define p (> g 6))

   (list g
         p
         )
   )
  )

(show-marginals (model)
                (list "g"
                      "p"
                        )
                  #:num-samples 10000
                  #:truncate-output 4
                  #:show-stats? #t
                  #:credible-interval 0.93
                  ; #:credible-interval2 0.93                  
                  ; #:skip-marginals? #t
                  ; #:show-histogram? #t
                  )
