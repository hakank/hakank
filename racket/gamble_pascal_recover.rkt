#| 

  Pascal distribution - recovering parameter in Racket/Gamble 

  From gamble_run_size_probability.rkt
  Generating 50 samples from the (exact) distribution of 
  the lengths of the lengths of the max run length for n=10 and m=2 (0..1):
   3: 185/512 (0.361328125)
   4: 127/512 (0.248046875)
   2: 11/64 (0.171875)
   5: 63/512 (0.123046875)
   6: 7/128 (0.0546875)
   7: 3/128 (0.0234375)
   8: 5/512 (0.009765625)
   9: 1/256 (0.00390625)
   1: 1/512 (0.001953125)
   10: 1/512 (0.001953125)



  Mathematica
  """
  xxx = {4, 3, 2, 3, 3, 3, 3, 3, 3, 3, 4, 2, 5, 2, 4, 4, 5, 2, 6, 4, 3, 
  3, 3, 2, 2, 3, 2, 4, 3, 3, 4, 3, 5, 3, 3, 3, 3, 4, 3, 3, 3, 3, 4, 3,
   4, 2, 2, 4, 3, 3}
  FindDistribution[xxx, 2]
  -> {BinomialDistribution[6, 0.541667], 
      PascalDistribution[2, 0.615385]}
  """

  data: (4 3 2 3 3 3 3 3 3 3 4 2 5 2 4 4 5 2 6 4 3 3 3 2 2 3 2 4 3 3 4 3 5 3 3 3 3 4 3 3 3 3 4 3 4 2 2 4 3 3)
  Min: 2 Mean: 3.22 Max: 6 Variance: 0.7716 Stddev: 0.87840765023991
  variable : n
  2: 1.0000000000000002

  variable : p
  0.6777619668900756: 0.3638463722387834
  0.7173849872833314: 0.36384637223878336
  0.6678959591169068: 0.08118509930706487
  0.7473439869784024: 0.04924125184154976
  0.7122836618224373: 0.02986632896453111
  ...
  0.36324368473882956: 1.7040365797791244e-291
  0.252505517788565: 5.680121932597081e-292
  0.2587798605936839: 5.680121932597081e-292
  0.31588689474640574: 1.4200304831492703e-292
  0.09307001492014862: 0.0
  mean: 0.7000987065001336
  HPD interval (0.84): 0.6938862180728713..0.7591644376458783
  HPD interval (0.9): 0.7194082396442154..0.8205401788572044
  HPD interval (0.99): 0.644363091788985..0.8205401788572044
  HPD interval (0.99999): 0.5845578774070943..0.881172299664797

  variable : post
  2: 0.9063916223628425
  3: 0.059766093390571685
  4: 0.022147759821612597
  5: 0.011608707149503354
  6: 8.172897901436091e-5
  ...
  29: 2.183573469391834e-229
  23: 2.0968157402083284e-252
  21: 1.589923205632648e-261
  31: 4.610393089213972e-286
  19: 5.680121932597081e-292
  mean: 2.1392382611513434
  HPD interval (0.84): 2..3
  HPD interval (0.9): 2..3
  HPD interval (0.99): 2..4
  HPD interval (0.99999): 2..7


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; This is the length of the lengths of the max run length for n=10 and m=2 (0..1)
;
(define data '{4 3 2 3 3 3 3 3 3 3 4 2 5 2 4 4 5 2 6 4 3 3 3
                 2 2 3 2 4 3 3 4 3 5 3 3 3 3 4 3 3 3 3 4 3 4 2
                 2 4 3 3})

; (define probs (vector 185/512 127/512 11/64 63/512 7/128 3/128 5/512 1/256 1/512 1/512))
; (define vals (vector 3 4 2 5 6 7 8 9 1 10))
; (define data (for/list ([i 50]) (categorical-vw2 probs vals)))

; (define data (for/list ([i 50]) (pascal_dist 2 0.615385)))
(show "data" data)
(show-stats data)

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define n (+ (min-list data) (random-integer (* 2 (max-list data)))))
   (define n (min-list data))
   (define p (beta 1 1))

   (for ([i (length data)])
     (observe-sample (normal-dist (pascal_dist n p) 1) (list-ref data i)))

   (define post (pascal_dist n p))
   
   (list n
         p
         post)
   )
)

(show-marginals (model)
                (list  "n"
                       "p"
                       "post"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.9 0.99 0.99999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


