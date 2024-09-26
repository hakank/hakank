#| 

  Greed for an ACE in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/209
  """
  What is the expected number of cards that need to be turned 
  over in a regular 52-card deck in order to see the first ace?

  Answer: 53/5 [10.6]
  """

  Importance-sampler (1000000 samples)

  var : first-ace
  1: 0.07654699999999999
  2: 0.07218899999999999
  3: 0.06803599999999999
  4: 0.06355999999999999
  5: 0.06005299999999999
  6: 0.05615399999999999
  7: 0.052714999999999984
  8: 0.04901299999999999
  9: 0.045558999999999995
  10: 0.04250999999999999
  ...
  40: 0.0008549999999999997
  41: 0.0005939999999999999
  42: 0.00047399999999999987
  43: 0.00030799999999999995
  44: 0.00022799999999999996
  45: 0.00011899999999999998
  46: 7.299999999999999e-5
  47: 3.7999999999999995e-5
  48: 1.1999999999999997e-5
  49: 3.999999999999999e-6
  mean: 10.600306


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define deck (flatten (for/list ([i (range 1 14)]) (ones-list 4 i))))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n (length deck))
   (define shuffled-deck (shuffle deck))
   ; (define shuffled-deck (draw-without-replacement n deck))

   (define first-ace (+ (index-of shuffled-deck 1) 1))

   (list first-ace)

   )
)

(show-marginals (model)
              (list  "first-ace"
                     )
                    #:num-samples 1000000
                    #:truncate-output 10
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


