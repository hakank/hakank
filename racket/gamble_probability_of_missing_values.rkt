#| 

  Probability of missing values in Racket.Gamble 

  From https://github.polettix.it/ETOOBUSY/2021/07/19/brute-force-puzzle/
  """
  Consider a display with N digits in base b

      A random value id shown on the display; each possible value is equiprobable. 
      What is the expected number of missing digit values?

  As an example, suppose that we consider base-4 digits (i.e. 0 to 3) and a display that 
  has 6 slots. An example value shown on the display would be 232133, which includes digits 1, 2, 
  and 3 but not digit 0. In this arrangement, digit 0 is considered missing. Similarly, in 
  arrangement 111111 there are three missing digits, namely 0, 2, and 3.

  ....

  (By hand and Brute force:)
  2916 / 4096 ≅ 0.711914  
  """
  
  This model agrees: 

  Testing base: 4 size: 6
  var : num_missing
  1: 135/256 (0.52734375)
  0: 195/512 (0.380859375)
  2: 93/1024 (0.0908203125)
  3: 1/1024 (0.0009765625)
  mean: 729/1024 (0.7119140625)


  Some other experiments:

  Testing base: 2 size: 2
  var : num_missing
  0: 1/2 (0.5)
  1: 1/2 (0.5)
  mean: 1/2 (0.5)


  Testing base: 3 size: 3
  var : num_missing
  1: 2/3 (0.6666666666666666)
  0: 2/9 (0.2222222222222222)
  2: 1/9 (0.1111111111111111)
  mean: 8/9 (0.8888888888888888)


  Testing base: 4 size: 4
  var : num_missing
  1: 9/16 (0.5625)
  2: 21/64 (0.328125)
  0: 3/32 (0.09375)
  3: 1/64 (0.015625)
  mean: 81/64 (1.265625)


  Testing base: 5 size: 5
  var : num_missing
  2: 12/25 (0.48)
  1: 48/125 (0.384)
  3: 12/125 (0.096)
  0: 24/625 (0.0384)
  4: 1/625 (0.0016)
  mean: 1024/625 (1.6384)


  Testing base: 6 size: 6
  var : num_missing
  2: 325/648 (0.5015432098765432)
  1: 25/108 (0.23148148148148148)
  3: 25/108 (0.23148148148148148)
  4: 155/7776 (0.01993312757201646)
  0: 5/324 (0.015432098765432098)
  5: 1/7776 (0.0001286008230452675)
  mean: 15625/7776 (2.0093878600823047)


  Testing base: 10 size: 5
  var : num_missing
  6: 63/125 (0.504)
  5: 189/625 (0.3024)
  7: 9/50 (0.18)
  8: 27/2000 (0.0135)
  9: 1/10000 (0.0001)
  mean: 59049/10000 (5.9049)

  Testing base: 10 size: 10 (using importance-sampler)
  var : num_missing
  3: 0.35415
  4: 0.34526
  2: 0.13759
  5: 0.12863
  6: 0.01741
  1: 0.01591
  7: 0.00063
  0: 0.00042
  mean: 3.4866


  This model is a port of my WebPPL model probability_of_missing_values.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model [base 4] [size 6])
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   ;; Each digit in 0..base-1
   (defmem (digit i) (random-integer base))

   ;; Collect the digits
   (define digits (for/list ([i size]) (digit i)))

   ;; Count the number of missing values
   (define num_missing (for/sum ([val base])
                          (if (= (for/sum ([d digits])
                                   (if (= d val) 1 0)) 0) 1 0)))
  
   (list num_missing
         ; digits
         )

   )
)

(displayln "\nTesting base: 4 size: 6")
(show-marginals (model)
                (list  "num_missing"
                       "digits"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


(displayln "\nTesting base: 10 size: 5")
(show-marginals (model 10 5)
                (list  "num_missing"
                       "digits"
                       )
                )

(for ([n (range 2 7)])
  (displayln (format "\nTesting base: ~a size: ~a" n n))
  (show-marginals (model n n)
                  (list  "num_missing"
                         "digits"
                         )
                  )
  )
  

#|
; Change to importance-sampler!
(displayln "\nTesting base: 10 size: 10 (using importance-sampler)")
(show-marginals (model 10 10)
                (list  "num_missing"
                       "digits"
                       )
                #:num-samples 100000
                )
|#
