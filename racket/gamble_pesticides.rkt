#| 

  Pesticides in Racket/Gamble 

  From Statistics101 (Resample Stats)
  http://www.statistics101.net/QuickReference.pdf
  Page 47
  """
  Here’s a more complicated example using resampling to perform a hypothesis test. The example
  is taken from Statistics the Easy Way by Douglas Downing and Jeffrey Clark, (Chapter 18
  exercise 5, page 223). Suppose four new pesticides are being tested in a laboratory, with the
  results shown in the following table, called a contingency table. Is pesticide 1 significantly 
  better than the rest?
                    Type 1 Type 2 Type 3 Type 4 Total
  Insects killed     139   100     73     98     410
  Insects surviving   15    50     80     47     192
  Total tested       154   150    153    145     602
  ...
  The result of this simulation is:
  probability: 0.0
  Out of 1000 trials from the null population there were no cases as extreme as the observed
  difference between ratios. Since that is well below our critical value (0.05), you can reject the
  null hypothesis.
  """

  variable : ratioDifference
  0.0726797385620915: 0.004999999999999997
  0.037093625328919445: 0.0039999999999999975
  0.04367816091954023: 0.0039999999999999975
  0.028526645768025077: 0.0039999999999999975
  0.06147186147186147: 0.0039999999999999975
  ...
  0.06256479603335587: 0.0009999999999999994
  0.08992562542258282: 0.0009999999999999994
  0.1326797385620915: 0.0009999999999999994
  0.13202614379084968: 0.0009999999999999994
  0.08675324675324675: 0.0009999999999999994
  mean: 0.07929308319357008
  HPD interval (0.84): 0.030344827586206897..0.12077922077922078

  variable : p
  #f: 1.0000000000000007
  mean: 0 (0.0)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define testRatioDifference 0.227)
(show "testRatioDifference" testRatioDifference)
(define nullPopulation (flatten (append (rep 410 "died") (rep 192 "survived"))))
(define typeNums '(154 150 153 145))

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define types (for/list ([v typeNums]) (resample v nullPopulation)))
   (define typeSurvivorCounts (for/list ([v types]) (count-occurrences-eq "survived" v)))
   (define ratioArray (for/list ([i (length typeNums)]) (/ (list-ref typeSurvivorCounts i) (list-ref typeNums i))))
   (define minRatio (apply min ratioArray))
   (define maxRatio (apply max ratioArray))

   (define ratioDifference (* 1.0 (- maxRatio minRatio)))
   ; What is the probability that we get the difference as large as from the test?
   (define p (>= ratioDifference testRatioDifference))
  
   (list ratioDifference
         p
    )
   
   )
)

(show-marginals (model)
                (list  "ratioDifference"
                       "p"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


