#| 

  Urn large balls in Racket.Gamble 

  From
  Siddharth Srivastava and Stuart Russell and Paul Ruan and Xiang Cheng
  "First-Order Open-Universe POMDPs"
  page 3
  """
  Fig. 1 shows a simple example of a BLOG model with two
  types, Urn and Ball. This model expresses a distribution
  over possible worlds consisting of varying numbers of urns
  with varying numbers of balls in each urn. The number of
  urns follows a Poisson(5) distribution (line 3). The number 
  of balls in an urn depends on whether or not the urn is
  Large. Origin functions map the object being generated to
  the arguments that were used in the number statement that
  was responsible for generating it. In Fig. 1, Source maps a
  ball to the urn it belongs to. The number of balls in an urn
  follows a Poisson(10) distribution if the urn is Large, and
  a Poisson(2) distribution otherwise (lines 4-6). Finally, the
  probability of an urn being Large is 0.5 (lines 7 & 8).
  """

  * Enumerate #:limit 1e-05
  5: 0.18518677003844072
  4: 0.17921300326300715
  6: 0.1568113778551313
  3: 0.13381237576971206
  7: 0.11289708042858088
  8: 0.07083847320198841
  2: 0.06881779325299478
  9: 0.0394318734272506
  10: 0.019735228236828446
  1: 0.018351411534131938
  11: 0.008974942726559082
  12: 0.003740472893793725
  13: 0.0014388190792272073
  14: 0.0005138953244628124
  15: 0.0001713036694202642
  16: 5.353321355831043e-5
  17: 1.1646084912083956e-5
  mean: 5.2234380447456426

  var : num_large_urns
  2: 0.2794574424380276
  3: 0.2328811703181275
  1: 0.22356595923601574
  4: 0.14555061756831117
  5: 0.07277500918709559
  6: 0.030322310358923864
  7: 0.01082840282224405
  8: 0.0033825819851357724
  9: 0.0009382493778386356
  10: 0.00023341356872406586
  11: 5.226277041395697e-5
  12: 1.0457171146895003e-5
  13: 1.825878545381334e-6
  14: 2.6487070949737414e-7
  15: 2.970984156925892e-8
  16: 2.6187714144017683e-9
  17: 1.2012712910099888e-10
  mean: 2.7235018998827685



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-05
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define num_urns (poisson 5))
   (observe/fail (> num_urns 0))

   (define (large_urn u) (flip 1/2))
    
   (define (num_balls u) (if (large_urn u) (poisson 10) (poisson 2)))

   (define num_large_urns (for/sum ([i num_urns]) (if (large_urn i) 1 0 )))

   (observe/fail (> num_large_urns 0))

   (list num_urns
         num_large_urns
    )


   )
)

(show-marginals (model)
                (list  "num_urns"
                       "num_large_urns"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


