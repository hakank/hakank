#| 

  Pepperoni pizza in Racket/Gamble 

  From Statistics101 (Resampling Stats)
  File pepperoniPizza.txt
  """
  If 24 pieces of sausage are randomly put onto a pizza that is sliced
  into 8 pieces (with none of the sausage pieces getting cut), what is the
  probability that your slice will have 3 pieces of sausage?
  From: "Statistics the Easy Way" Downing & Clark, 3rd edition, p. 58.
  -> 
  probability: 0.2412
  probability: 0.2338
  probability: 0.239532
  """

  variable : c
  3: 0.24000000000000002
  4: 0.20270000000000002
  2: 0.18900000000000003
  5: 0.13110000000000002
  1: 0.09450000000000001
  6: 0.07060000000000001
  7: 0.030300000000000004
  0: 0.025900000000000003
  8: 0.011200000000000002
  9: 0.004200000000000001
  10: 0.0004000000000000001
  12: 0.00010000000000000002
  mean: 3.4271
  HPD interval (0.84): 1..5

  variable : p
  #f: 0.76
  #t: 0.24000000000000002
  mean: 0.24000000000000002


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

   (define num_pieces 24)
   (define num_slices 8)
   ; Place 24 pieces of sausage (randomly) on the 8 slices
   (define pizza (resample num_pieces  (range 1 8)))
   ; How many sausages are on my slice (slice 1)?
   (define c (count-occurrences 1 pizza))
   (define p (= c 3)) ; Did I get 3 sausages?
   (list c
         p
         )
   )
)

(show-marginals (model)
                (list  "c"
                       "p"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


