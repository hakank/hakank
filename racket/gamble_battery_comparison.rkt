#| 

  Battery comparison in Racket.Gamble 

  From "Resampling Stats Illustrations" (http://www.statistics101.net/PeterBruce_05-illus.pdf)
  Page 40
  """
  Hypothesis test for a difference in means
  (program “battery”)
  Does one brand of car battery last longer than another? Here are
  the figures on 10 randomly selected batteries each for brand A and
  brand B. Is brand A’s apparent advantage significant?
  TABLE 2. BATTERY LIFE TIMES
  brand life (months) aveage
  A 30 32 31 28 31 29 29 24 30 31 29.5
  B 28 28 32 31 24 23 31 27 27 31 28.2
  A’s advantage: 1.3 months
  ...
  We calculate the average life time of each group, and determine whether 
  they differ by as much as the observed data.
  ->
  prob = .147
  """

  Here are three models which differs how the samples are generated.
  "p" is the probability that type a batteries are better 
  than type b + 1.3 (the original difference of means)


  This is a port of my WebPPL model battery_comparison.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define type_a '(30 32 31 28 31 29 29 24 30 31 29.5))
(define type_b '(28 28 32 31 24 23 31 27 27 31 28.2))
(define orig_diff (- (avg type_a) (avg type_b)))
(show2 "type a mean:" (avg type_a) "type_b mean:" (avg type_b) "diff:" orig_diff)

(define both_types (append type_a type_b))

 
(define (model1)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ;; Draw 10 samples from the combined data
   (define a_sample_mean (avg (resample (length both_types) type_a)))
   (define b_sample_mean (avg (resample (length both_types) type_b)))
   (define diff (- a_sample_mean b_sample_mean))
   (define p (>= diff orig_diff))
   (define p1 (> a_sample_mean b_sample_mean))
   (define p2 (> b_sample_mean a_sample_mean))

   (list a_sample_mean
         b_sample_mean
         diff
         p
         p1
         p2
    )

   )
)

#|
  Model 1 
  
  (type a mean: 29.5 type_b mean: 28.2 diff: 1.3000000000000007)
  Model 1
  var : a_sample_mean
  mean: 29.475636363636344
  Credible interval (0.95): 28.65909090909091..30.272727272727273

  var : b_sample_mean
  mean: 28.220190909090874
  Credible interval (0.95): 27.00909090909091..29.281818181818185

  var : diff
  mean: 1.2554454545454548
  Credible interval (0.95): -0.14545454545454461..2.745454545454546

  var : p
  mean: 0.4910000000000004
  Credible interval (0.95): 0..1

  var : p1
  mean: 0.9530000000000007
  Credible interval (0.95): 1..1

  var : p2
  mean: 0.047000000000000014
  Credible interval (0.95): 0..0


|#
(displayln "Model 1")
(show-marginals (model1)
                (list  "a_sample_mean"
                       "b_sample_mean"
                       "diff"
                       "p"
                       "p1"
                       "p2"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.95
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )

#|
  Alternative version: Drawing without replacement (see page 42f)

  var : a_sample_mean
  mean: 28.854327272727254
  Credible interval (0.95): 27.7..29.954545454545453

  var : b_sample_mean
  mean: 28.845672727272703
  Credible interval (0.95): 27.745454545454546..30

  var : diff
  mean: 0.008654545454545366
  Credible interval (0.95): -2.3000000000000007..2.209090909090907

  var : p
  mean: 0.1359999999999999
  Credible interval (0.95): 0..1

  var : p1
  mean: 0.517
  Credible interval (0.95): 0..1

  var : p2
  mean: 0.48300000000000004
  Credible interval (0.95): 0..1


|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define a_len (length type_a))
   (define len (length both_types))
   
    ;; Draw without replacement from the the concated array
   (define both_sample (draw-without-replacement len both_types))
   ;; Pick the first a_len elements -> a_sample
   (define a_sample_mean (avg (take both_sample a_len)))
   ;; Pick the rest -> b_sample
   (define b_sample_mean (avg (drop both_sample a_len)))
   
   (define diff (- a_sample_mean b_sample_mean))
   (define p (>= diff orig_diff))
   (define p1 (> a_sample_mean b_sample_mean))
   (define p2 (> b_sample_mean a_sample_mean))
   
   (list a_sample_mean
         b_sample_mean
         diff
         p
         p1
         p2
         )

   )
)

(displayln "Model 2")
(show-marginals (model2)
                (list  "a_sample_mean"
                       "b_sample_mean"
                       "diff"
                       "p"
                       "p1"
                       "p2"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.95
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


#|
  Alternative version:
  Instead of concatenating the data we keep the two types separately.

  Using this approach, we see that type b have a small chance of being better
  than type a.

  var : a_sample_mean
  mean: 29.486409090909067
  Credible interval (0.95): 310/11..30.59090909090909

  var : b_sample_mean
  mean: 28.17901818181815
  Credible interval (0.95): 26.745454545454546..29.763636363636362

  var : diff
  mean: 1.3073909090909077
  Credible interval (0.95): -0.6363636363636367..3.3181818181818166

  var : p
  mean: 0.5170000000000003
  Credible interval (0.95): 0..1

  var : p1
  mean: 0.8920000000000007
  Credible interval (0.95): 0..1

  var : p2
  mean: 0.10600000000000007
  Credible interval (0.95): 0..1


|#
(define (model3)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define a_sample_mean (avg (resample (length type_a) type_a)))
   (define b_sample_mean (avg (resample (length type_b) type_b)))   
   
   (define diff (- a_sample_mean b_sample_mean))
   (define p (>= diff orig_diff))
   (define p1 (> a_sample_mean b_sample_mean))
   (define p2 (> b_sample_mean a_sample_mean))
   
   (list a_sample_mean
         b_sample_mean
         diff
         p
         p1
         p2
         )

   )
)

(displayln "Model 3")
(show-marginals (model3)
                (list  "a_sample_mean"
                       "b_sample_mean"
                       "diff"
                       "p"
                       "p1"
                       "p2"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    #:credible-interval 0.95
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


