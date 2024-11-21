#| 

  Nine spades and four clubs in Racket/Gamble 

  From Statistics101
  """
  Compute the probability of getting in a 13 card hand: 
  Nine cards to be spades and four to be clubs. Order
  does not matter.
  From Resampling: The New Statistics, Julian Simon p. 125
  (Assumes that the numbers 1 through 13 represent spades.)
  """

  variable : num_spades
  3: 0.25087000000000015
  4: 0.2106900000000001
  2: 0.2073500000000001
  5: 0.12592000000000006
  1: 0.10167000000000001
  ...
  7: 0.017890000000000007
  8: 0.00447
  9: 0.0006600000000000002
  10: 0.00017000000000000007
  11: 1.0000000000000004e-5
  mean: 3.2514200000000013

  variable : num_clubs
  3: 0.24882000000000012
  4: 0.20953000000000008
  2: 0.20756000000000005
  5: 0.12562000000000006
  1: 0.10376000000000002
  ...
  0: 0.023670000000000007
  7: 0.019350000000000006
  8: 0.00464
  9: 0.0008300000000000002
  10: 0.00014000000000000004
  mean: 3.2494800000000015

  variable : p
  #f: 0.9999999999999997
  mean: 0 (0.0)

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

   ; spades: 1, clubs: 2 (and we don't care about 3 and 4 representing diamonds and heart)
   (define cards (for/list ([i 13]) (add1 (random-integer 4))))

   (define num_spades (count-occurrences 1 cards))
   (define num_clubs (count-occurrences 2 cards))
   (define p (and (= num_spades 9) (= num_clubs 4)))
   (list num_spades
         num_clubs
         p
         )
   )
)

(show-marginals (model)
                (list  "num_spades"
                       "num_clubs"
                       "p"
                       )
                #:num-samples 100000
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


