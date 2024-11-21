#| 

  Generating Gumbel dist in Racket/Gamble 

  From Handbook on probability distributions
  page 111ff
  and Mathematica.
  
  See gamble_distributions.rkt and gamble_distributions_test.rkt for more on this.

  var : g1
  mean: -0.5756461260756904
  HPD interval (0.5): -0.8695816223246753..0.6127839559270641
  HPD interval (0.84): -2.089516656146542..1.1509018264941648
  HPD interval (0.95): -3.1402920833288244..1.5878310681699208
  HPD interval (0.99): -4.799533195636655..1.8735934269210095

  var : g2
  mean: -0.09081159997122307
  HPD interval (0.5): -0.288664590845782..1.192570456134046
  HPD interval (0.84): -1.541164375319914..1.6874737857600888
  HPD interval (0.95): -2.7340611808756132..2.0118888066356684
  HPD interval (0.99): -4.276145858322647..2.4175685323106535

  var : g3
  mean: -0.20356174567909455
  HPD interval (0.5): -0.6211781168199091..2.328062374794384
  HPD interval (0.84): -3.0397906774163177..3.41565726659939
  HPD interval (0.95): -5.412018700823551..4.024829622363072
  HPD interval (0.99): -7.9318373353188605..4.899880342533285

  var : g4
  mean: 4.223201239600031
  HPD interval (0.5): 2.063309885933215..16.657985620507226
  HPD interval (0.84): -10.682002755694164..21.371704821662867
  HPD interval (0.95): -21.687481684130915..25.06536573653984
  HPD interval (0.99): -36.252787337323085..29.225571363170975

  (gumbel_dist_mean 0 1): -0.5772156649015329
  (gumbel_dist_mean 1/2 1): -0.07721566490153287
  (gumbel_dist_mean 1 2): -0.15443132980306573
  (gumbel_dist_mean 10 10): 4.227843350984672

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
   
   (define g1 (gumbel_dist 0 1))
   (define g2 (gumbel_dist 1/2 1))
   (define g3 (gumbel_dist 1 2))
   (define g4 (gumbel_dist 10 10))   
   
   (list g1
         g2
         g3
         g4)
   
   )
)

(show-marginals (model)
                (list  "g1"
                       "g2"
                       "g3"
                       "g4")
                
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.5
                #:hpd-interval (list 0.5 0.84 0.95 0.99)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(show "(gumbel_dist_mean 0 1)" (gumbel_dist_mean 0 1))
(show "(gumbel_dist_mean 1/2 1)" (gumbel_dist_mean 1/2 1))
(show "(gumbel_dist_mean 1 2)" (gumbel_dist_mean 1 2))
(show "(gumbel_dist_mean 10 10)" (gumbel_dist_mean 10 10))
