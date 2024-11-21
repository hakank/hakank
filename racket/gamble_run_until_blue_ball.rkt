#| 

  Run until blue ball in Racket/Gamble 

  From https://community.wolfram.com/groups/-/m/t/3007279
  """
  Experiment
  1. Start with 2 balls in urn: 1 red and 1 blue
  2. If you pick a blue ball, terminate. If red, put the red ball back and add one red ball to the urn
  3. Repeat until you pick a blue ball
  Find the average length of trials.
  """

  * Model 1: Exact probabilities given a certain length

  limit: 1
  (mean 2.0)

  limit: 10
  (mean 3.9289682539682538)

  limit: 100
  (mean 6.187377517639621)

  limit: 1000
  (mean 8.485470860550345)

  limit: 10000
  (mean 10.787606036044382)

  limit: 100000
  (mean 13.090146129863427)

  More detailed result For limit 10000 (using show-marginals)
  variable : len
  2: 1/2 (0.5)
  3: 1/6 (0.16666666666666666)
  4: 1/12 (0.08333333333333333)
  5: 1/20 (0.05)
  6: 1/30 (0.03333333333333333)
  7: 1/42 (0.023809523809523808)
  8: 1/56 (0.017857142857142856)
  9: 1/72 (0.013888888888888888)
  10: 1/90 (0.011111111111111112)
  11: 1/110 (0.00909090909090909)
  ...
  9991: 1/99810090 (1.0019027134430998e-8)
  9992: 1/99830072 (1.0017021724676308e-8)
  9993: 1/99850056 (1.0015016916965975e-8)
  9994: 1/99870042 (1.0013012711059038e-8)
  9995: 1/99890030 (1.0011009106714654e-8)
  9996: 1/99910020 (1.0009006103692102e-8)
  9997: 1/99930012 (1.0007003701750782e-8)
  9998: 1/99950006 (1.0005001900650212e-8)
  9999: 1/99970002 (1.0003000700150031e-8)
  10000: 1/99990000 (1.000100010001e-8)

  mean: ... (10.787606036044382)


  * Model 2: No limit with importance-sampler 

  - 10000 samples: 
  variable : len
  2: 0.497600000000001
  3: 0.16880000000000034
  4: 0.08380000000000017
  5: 0.053100000000000105
  6: 0.03400000000000007
  ...
  230: 0.00010000000000000021
  486: 0.00010000000000000021
  237: 0.00010000000000000021
  2040: 0.00010000000000000021
  255: 0.00010000000000000021
  mean: 10.321200000000012

  - 100000 samples
  variable : len
  2: 0.49984000000000595
  3: 0.16682000000000197
  4: 0.08305000000000098
  5: 0.0506400000000006
  6: 0.033490000000000394
  ...
  996: 1.000000000000012e-5
  1006: 1.000000000000012e-5
  8177: 1.000000000000012e-5
  2043: 1.000000000000012e-5
  1022: 1.000000000000012e-5
  mean: 12.725510000000154

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

; Enumerate with limit
(define (model1 limit)
  (enumerate

   (define num_to_add 1)
   
   ; (define limit 100)
   (define (f num_red)
     (if (>= num_red limit)
         num_red
         (let ([pct (/ num_red (add1 num_red))])
           (if (flip pct)
               (f (+ num_red num_to_add))
               num_red))))
     
   ; We start with one red (and one blue)
   (define len (add1 (f 1))) ; add the blue ball
   (list len
    )

   )
)

#|
; Faster way to calculate the (exact) mean
(for ([limit (range 1 6)])
  (show "limit" (expt 10 limit))
(let* ([res (model1 (expt 10 limit))]
       [vals (map first (vector->list (discrete-dist-values res)))]
       [weights (vector->list (discrete-dist-weights res))]
       [mean-val (scalar-product vals weights)])
  (show2 "mean" (* 1.0 mean-val))
  )
)
|#

; For detailed view of the result
(show-marginals (model1 10000)
              (list  "len")
              #:truncate-output 10
              ; #:skip-marginals? #t
              )

(define (model2)
  (importance-sampler
   
   (define num_to_add 1)
   
   (define (f num_red)
     (let ([pct (/ num_red (add1 num_red))])
       (if (flip pct)
           (f (+ num_red num_to_add))
           num_red)))
  
   ; We start with one red (and one blue)
   (define len (add1 (f 1))) ; add the blue ball
   (list len
         )

   )
)

(displayln "\nModel 2")
(show-marginals (model2)
              (list  "len"
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

#|
(for ([limit (range 1 7)])
  (newline)
  (show "limit" (expt 10 limit))
(let* ([res (time (sampler->discrete-dist (model2) (expt 10 limit)))]
       [vals (map first (vector->list (discrete-dist-values res)))]
       [weights (vector->list (discrete-dist-weights res))]
       [mean-val (scalar-product vals weights)])
  (show2 "mean" (* 1.0 mean-val))
  )
)
|#


#|
  Adding 2 instead
  Enumerate with limit 10000


  variable : len
  2: 1/2 (0.5)
  4: 1/8 (0.125)
  6: 1/16 (0.0625)
  8: 5/128 (0.0390625)
  10: 7/256 (0.02734375)
  12: 21/1024 (0.0205078125)
  14: 33/2048 (0.01611328125)
  16: 429/32768 (0.013092041015625)
  18: 715/65536 (0.0109100341796875)
  20: 2431/262144 (0.009273529052734375)
  22: 4199/524288 (0.008008956909179688)
  10002: ... (0.007978646139382154)
  24: 29393/4194304 (0.0070078372955322266)
  26: 52003/8388608 (0.006199240684509277)
  ...
  mean: ...  (159.58888007992184)

|#
(define (model3)
  (enumerate

   (define num_to_add 2)

   (define limit 10000)
   
   ; (define limit 100)
   (define (f num_red)
     (if (>= num_red limit)
         num_red
         (let ([pct (/ num_red (add1 num_red))])
           (if (flip pct)
               (f (+ num_red num_to_add))
               num_red))))
     
   ; We start with one red (and one blue)
   (define len (add1 (f 1))) ; add the blue ball
   ; (observe/fail (< len limit))
   (list len
         limit
    )

   )
)

(displayln "Model 3")
(show-marginals (model3)
              (list  "len" "limit")
              ; #:truncate-output 10
              ; #:skip-marginals? #t
              )
