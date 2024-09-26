#| 

  Dice 6 throws in Racket/Gamble 

  http://cplint.eu/example/inference/dice.swinb
  """
  A six-sided die is repeatedly thrown until the outcome is six. 
  on(T,F) means that on the Tth throw the face F came out.
  """

  What is the probability that the die lands on face 1 at time 0?

  Cf gamble_dice_6_throws.rkt
  This is an alternative approach using arrays instead.

  var : len
  1: 0.16687000000000007
  2: 0.13923000000000008
  3: 0.11525000000000005
  4: 0.09810000000000005
  5: 0.07925000000000004
  6: 0.06720000000000002
  7: 0.05502000000000003
  8: 0.04621000000000003
  9: 0.03894000000000001
  10: 0.03217000000000001
  11: 0.026210000000000015
  12: 0.02242000000000001
  13: 0.018480000000000007
  14: 0.015650000000000004
  15: 0.012890000000000006
  16: 0.010950000000000007
  17: 0.009180000000000002
  18: 0.007850000000000003
  19: 0.006910000000000004
  20: 0.005180000000000002
  21: 0.004090000000000001
  22: 0.003360000000000002
  23: 0.002970000000000001
  24: 0.0028200000000000013
  25: 0.0022100000000000006
  26: 0.001780000000000001
  27: 0.0015200000000000007
  28: 0.0011300000000000004
  29: 0.0011000000000000007
  31: 0.0008100000000000004
  30: 0.0008000000000000005
  32: 0.0006400000000000004
  33: 0.0004800000000000003
  34: 0.0003900000000000002
  35: 0.0003700000000000002
  36: 0.00033000000000000016
  37: 0.0002300000000000001
  38: 0.0001800000000000001
  40: 0.00017000000000000007
  39: 0.00014000000000000007
  42: 9.000000000000005e-5
  43: 9.000000000000005e-5
  45: 9.000000000000005e-5
  47: 4.0000000000000024e-5
  48: 4.0000000000000024e-5
  44: 3.0000000000000018e-5
  49: 3.0000000000000018e-5
  52: 3.0000000000000018e-5
  46: 2.0000000000000012e-5
  77: 1.0000000000000006e-5
  41: 1.0000000000000006e-5
  50: 1.0000000000000006e-5
  56: 1.0000000000000006e-5
  57: 1.0000000000000006e-5
  61: 1.0000000000000006e-5
  mean: 6.004630000000003

  var : (a 0) = 1
  #f: 0.83618
  #t: 0.16382000000000016
  mean: 0.16382000000000016

  var : (a 1) = 1
  #f: 0.8625599999999999
  #t: 0.13744000000000026
  mean: 0.13744000000000026

  var : (a 2) = 1
  #f: 0.8819799999999998
  #t: 0.11802
  mean: 0.11802

  var : p
  #t: 0.9999999999999998
  mean: 0.9999999999999998



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
   
   (define (throwDice t) (uniform-draw '(1 2 3 4 5 6)))
   
   ;; Throw until the outcome is a 6
   (defmem (until6 arr)     
     (if (and (> (length arr) 0) (= (last arr) 6))
         arr
         (let ([v (throwDice (length arr))])
           (until6 (append arr (list v))))))
   
   (define a (until6 '()))
   (define len (length a))
   (define p (= (last a) 6))
   
   (list (length a)        
         (= (list-ref a 0) 1)
         (if (and (>= len 2) (= (list-ref a 1) 1)) #t #f)         
         (if (and (>= len 3) (= (list-ref a 2) 1)) #t #f)
         p
         )
   
   )
)

(show-marginals (model)
                (list  "len"                      
                       "(a 0) = 1"
                       "(a 1) = 1"
                       "(a 2) = 1"
                       "p"                       
                     )
                    #:num-samples 100000
                    )


