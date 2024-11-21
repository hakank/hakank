#| 

  Size of material in Racket/Gamble 

  https://pedrozudo.github.io/docs/20_thesis/thesis_final_online_compressed.pdf
  page 100
  """
  Example 6.11. We model the size of a ball as a mixture of different beta distributions,
  depending on whether the ball is made out of wood or metal (Line 1)4. We would
  now like to know the probability of the ball being made out of wood given that we
  have a measurement of the size of the ball. In order to condition on a continuous
  random variable we introduce the observation/2 predicate, which has an analogous
  functionality as the evidence predicates for Boolean random variables.
  1 3/10::material(wood);7/10::material(metal).
  2
  3 size~beta(2,3):-material(metal).
  4 size~beta(4,2):-material(wood).
  5
  6 observation(size,4/10).
  7 query(material(wood)).
  """

  variable : material
  metal: 0.8414697463886166
  wood: 0.15853025361110878

  variable : size
  0.44842089929846185: 0.0001198162817013689
  0.2834012271667108: 0.0001198162817013689
  0.6675431787071032: 0.0001198162817013689
  0.4111832008538184: 0.0001198162817013689
  0.5113190415843019: 0.0001198162817013689
  ...
  0.6270128426471355: 5.325168075616396e-5
  0.8189338269386344: 5.325168075616396e-5
  0.2975198972278208: 5.325168075616396e-5
  0.8592213103331111: 5.325168075616396e-5
  0.6095651106950914: 5.325168075616396e-5
  mean: 0.4436424654106472


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define material_type (vector "wood" "metal"))
   (define material (categorical-vw2 (vector 3/10 7/10) material_type))
   (define size (if (eq? material "metal") (beta-dist 2 3) (beta-dist 4 2)))

   (observe-sample size 4/10)

   (list material
         (sample size)
         )
   )
)

(show-marginals (model)
                (list  "material"
                       "size"
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


