#| 

  Shifted exponential dist in Racket/Gamble 

  From Handbook on probability distributions
  page 59ff

  var : d
  3.676236591988305: 0.00010000000000000938
  1.6505659785114002: 0.00010000000000000938
  5.15450253298309: 0.00010000000000000938
  11.006104009962634: 0.00010000000000000938
  1.7720291960166032: 0.00010000000000000938
  ...
  2.1868416572720557: 0.00010000000000000938
  2.2504252883156117: 0.00010000000000000938
  2.7009560636067276: 0.00010000000000000938
  2.1747145811709485: 0.00010000000000000938
  2.126552131943363: 0.00010000000000000938
  mean: 4.9434829180923465
  Min: 1.0003646238359962 Mean: 4.982526792377543 Max: 35.43592699928123 Variance: 15.986917849195374 Stddev: 3.9983643967496727
  Credible interval (0.84): 1.0003646238359962..8.28846187510412


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

   (define lambda_ 1/4)
   (define t 1)
   (define d (shifted_exponential lambda_ t))

   (list d
         )
   )
)

(show-marginals (model)
              (list  "d"
                     )
              #:num-samples 10000
              #:truncate-output 5
              ; #:skip-marginals? #t
              #:show-stats? #t
              #:credible-interval 0.84
              ; #:show-histogram? #t
              ; #:show-percentiles? #t
              )


