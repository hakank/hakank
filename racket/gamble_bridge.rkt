#| 

  Bridge problem in Racket.Gamble 

  From Statistics101 (Resampling Stats)
  """
  What is the probability of getting a total
  of 15 points in a bridge hand when
  ace = 4, king =3, queen = 2, and jack = 1.
  From Simon, "The New Statistics", p 126

  -> probability: 0.04404
  """

  var : s
  8: 0.0884
  9: 0.0837
  10: 0.082
  11: 0.0752
  12: 0.0728
  ...
  27: 0.0008
  28: 0.0003
  32: 0.0002
  30: 0.0002
  29: 0.0001
  mean: 9.946900000000001

  var : p
  #f: 0.9601
  #t: 0.0399
  mean: 0.0399


  This is a port of my WebPPL model bridge.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define deck (append (rep 4 4) (rep 4 3) (rep 4 2) (rep 4 1) (rep 36 0)))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define hand (resample 13 deck))
   (define s (sum hand))
   (define p (= s 15))
   
   (list s
         p
         )
   
   )
)

(show-marginals (model)
                (list  "s"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

