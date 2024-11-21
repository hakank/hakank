#| 

  Pascal distribution - number of fair coin flips before n heads in Racket/Gamble 

  From Mathematica PascalDistribution
  """
  The number of fair coin flips before 3 heads:
  heads3 = PascalDistribution[3, 1/2];
  ...
  Find the probability of getting 3 heads in no more than 6 flips:
  Probability[x <= 6, x \[Distributed] heads3]
  -> 
  21/32
  0.65625

  Find the average number of flips before getting 3 heads:
  Mean[heads3]
  -> 
  6
  """

  (pascal_cdf 3 1/2 6): : 0.6562499999999996
  (pascal_mean 3 1/2): : 6

  variable : len
  4: 0.18906000000000006
  5: 0.18516000000000002
  6: 0.15616000000000002
  3: 0.12630000000000002
  7: 0.11693000000000003
  8: 0.08153000000000002
  9: 0.05445000000000001
  10: 0.03554000000000001
  11: 0.022560000000000004
  12: 0.013250000000000003
  13: 0.007580000000000002
  14: 0.004780000000000001
  15: 0.0027900000000000004
  16: 0.0016600000000000005
  17: 0.0010500000000000002
  18: 0.0004900000000000001
  19: 0.00027000000000000006
  20: 0.00021000000000000006
  21: 8.000000000000002e-5
  22: 8.000000000000002e-5
  23: 3.0000000000000008e-5
  24: 2.0000000000000005e-5
  25: 1.0000000000000003e-5
  27: 1.0000000000000003e-5
  mean: 5.996260000000002
  HPD interval (0.84): 3..8
  HPD interval (0.9): 3..9
  HPD interval (0.99): 3..14
  HPD interval (0.99999): 3..31

  variable : p
  #t: 0.6566800000000002
  #f: 0.3433200000000001
  mean: 0.6566800000000002


  Compare with the PDF:
  0: 0
  1: 0
  2: 0
  3: 0.125
  4: 0.1875
  5: 0.1875
  6: 0.15625
  7: 0.1171875
  8: 0.08203125
  9: 0.0546875
  10: 0.03515625
  11: 0.02197265625
  12: 0.013427734375
  13: 0.008056640625
  14: 0.0047607421875
  15: 0.002777099609375
  16: 0.0016021728515625
  17: 0.00091552734375
  18: 0.000518798828125
  19: 0.0002918243408203125
  20: 0.00016307830810546875
  21: 9.059906005859375e-5
  22: 5.0067901611328125e-5
  23: 2.753734588623047e-5
  24: 1.5079975128173828e-5
  25: 8.225440979003906e-6
  26: 4.470348358154297e-6
  27: 2.421438694000244e-6
  28: 1.3075768947601318e-6
  29: 7.040798664093018e-7

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

(show "(pascal_cdf 3 1/2 6): " (pascal_cdf 3 1/2 6))
(show "(pascal_mean 3 1/2): " (pascal_mean 3 1/2))
(newline)
(for ([k (range 30)])
  (show k (pascal_pdf 3 0.5 k)))
      
(newline)

(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define (f a)
     (if (>= (sum a) 3)
         a
         (f (append a (list (bernoulli 1/2))))))

   (define a (f '()))
   (define len (length a))

   (define p (<= len 6))
  
   (list len
         p)

   )
)

(show-marginals (model)
                (list  "len"
                       "p"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84 0.9 0.99 0.99999)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


(newline)

#|
  From Mathematica PascalDistribution
  """
  A coin was flipped 10 times and the 7th head occurred at the 10th flip. 
  Find the probability of such an event if the coin is fair:

  fairD = PascalDistribution[7, 1/2]
  Probability[x == 10, x in fairD]
  -> 
  21/256 
  0.0820313  
  """

  > (pascal_pdf 7 1/2 10)
  21/256

  (pascal_pdf 7 1/2 10): 21/256

  variable : pos-7
  0: 53/64 (0.828125)
  9: 21/256 (0.08203125)
  8: 7/128 (0.0546875)
  7: 7/256 (0.02734375)
  6: 1/128 (0.0078125)
  mean: 181/128 (1.4140625)
  HPD interval (0.84): 0..7
  HPD interval (0.9): 0..8
  HPD interval (0.99): 0..9
  HPD interval (0.99999): 0..9

  variable : sum-x
  5: 63/256 (0.24609375)
  4: 105/512 (0.205078125)
  6: 105/512 (0.205078125)
  3: 15/128 (0.1171875)
  7: 15/128 (0.1171875)
  2: 45/1024 (0.0439453125)
  8: 45/1024 (0.0439453125)
  1: 5/512 (0.009765625)
  9: 5/512 (0.009765625)
  0: 1/1024 (0.0009765625)
  10: 1/1024 (0.0009765625)
  mean: 5 (5.0)
  HPD interval (0.84): 3..7
  HPD interval (0.9): 2..7
  HPD interval (0.99): 1..9
  HPD interval (0.99999): 0..9

  variable : p
  #f: 235/256 (0.91796875)
  #t: 21/256 (0.08203125)
  mean: 21/256 (0.08203125)

|#

(define (model2)
  (enumerate

   (define n 10)
   (define t 7)
   
   (define x (for/list ([i n]) (bernoulli 1/2)))

   (define sum-x (sum x))
  
   ; Position of the h'th head
   ;; (define (pos_x xs h)
   ;;   (define (loop c p)
   ;;     (cond
   ;;       [(>= p (length xs)) 0]
   ;;       [(= c h) p]
   ;;       [else (loop (if (= 1 (list-ref xs p)) (add1 c) c)
   ;;                   (add1 p))]))
   ;;   (loop 0 1) ; 1-based pos
   ;;   )

   ; Simpler
   (define (pos xs h)
     (let ([ix (index-of (accum xs) h)])
       (if (not ix) 0 ix)))
   
   (define pos-7 (pos x t)) ; 1-based
  
   (define p (= pos-7 9)) ; 0-based
  
   (list pos-7
         sum-x
         p)

   )
)

(displayln "\nModel 2")

(show "(pascal_pdf 7 1/2 10)" (pascal_pdf 7 1/2 10))
(newline)
(show-marginals (model2)
                (list  "pos-7"
                       "sum-x"
                       "p"
                       )
                #:hpd-interval (list 0.84 0.9 0.99 0.99999))



#|
  Mathematica PascalDistribution

  """
  Assuming the coin may not be fair, find the most likely value for p:

  d = PascalDistribution[7, p];
  FindMaximum[PDF[\[ScriptCapitalD], 10], {p, 0.9}]
  ->
  {0.18678, {p -> 0.7}}
  """

  Here's another take on this, with a little different result (p=0.667)

  variable : p
  0.6487763484647304: 1.0000000000019164e-5
  0.7403171980567465: 1.0000000000019164e-5
  0.6197276677482125: 1.0000000000019164e-5
  0.7835019445885321: 1.0000000000019164e-5
  0.6258625691173442: 1.0000000000019164e-5
  ...
  0.8201182456500553: 1.0000000000019164e-5
  0.7366627870763788: 1.0000000000019164e-5
  0.6266023204338388: 1.0000000000019164e-5
  0.603332842905186: 1.0000000000019164e-5
  0.7113422671816093: 1.0000000000019164e-5
  mean: 0.6667581618180256
  HPD interval (0.84): 0.49368496505452547..0.8611973044049115
  HPD interval (0.9): 0.4569173703415262..0.8812515488692851
  HPD interval (0.99): 0.32762579564459715..0.9427815801956427
  HPD interval (0.99999): 0.130588578518067..0.9893733348052136

  variable : post
  13: 0.1206100000000001
  12: 0.11831000000000015
  14: 0.1084400000000001
  11: 0.10096000000000012
  15: 0.09409000000000009
  ...
  88: 1.0000000000000011e-5
  89: 1.0000000000000011e-5
  92: 1.0000000000000011e-5
  93: 1.0000000000000011e-5
  63: 1.0000000000000011e-5
  mean: 15.690570000000013
  HPD interval (0.84): 10..20
  HPD interval (0.9): 10..22
  HPD interval (0.99): 10..34
  HPD interval (0.99999): 10..142


  With rounding p to 0.01 precision
  variable : p

  0.7000000000000001: 0.02942000000000003
  0.6900000000000001: 0.029270000000000025
  0.72: 0.02926000000000003
  0.68: 0.02911000000000002
  0.67: 0.02906000000000003
  ...
  0.19: 7.000000000000008e-5
  0.17: 3.000000000000003e-5
  0.16: 1.0000000000000011e-5
  0.99: 1.0000000000000011e-5
  0.15: 1.0000000000000011e-5
  mean: 0.6667928000000006
  HPD interval (0.84): 0.49..0.85
  HPD interval (0.9): 0.45..0.87
  HPD interval (0.99): 0.32..0.93
  HPD interval (0.99999): 0.09..0.99

|#

(define (model3)
  (importance-sampler

   (define n 10)
   (define t 7)

   ; p is unknown and we try to restore it
   (define p (beta 1 1))
   ; (define p (roundf (beta 1 1) 0.01)) ; round to 0.01 precision
   (define x (for/list ([i n]) (bernoulli p)))

   (define (accum-pos xs h)
     (let ([ix (index-of (accum xs) h)])
       (if (not ix) 0 ix)))
   
   (define pos-7 (accum-pos x t)) ; 1-based
   
   (observe/fail (eq? pos-7 9)) ; 0-based

   (define post (pascal_dist n p))
   
   (list pos-7
         p
         post)

   )
)

(displayln "\nModel 3")

(show-marginals (model3)
                (list  "pos-7"
                       "p"
                       "post"
                       )
                #:num-samples 100000
                #:truncate-output 10
                ; #:show-histogram? #t
                #:hpd-interval (list 0.84 0.9 0.99 0.99999))


#|
  Same problem as in model3 but we use pascal_pdf to get the optimal p:

  argmax2:: 0.7
  argmax2-val: (700 0.1867795524)

  0.65: 0.1765537374808594
  0.66: 0.18010409440321623
  0.67: 0.18295518608681385
  0.68: 0.18505107398599902
  0.6900000000000001: 0.1863408201882183
  0.7000000000000001: 0.18677955240000002 <----
  0.7100000000000001: 0.18632954385611838
  0.7200000000000001: 0.1849612940029093
  0.7300000000000001: 0.1826545938431644
  0.7400000000000001: 0.17939955774608263
  0.7500000000000001: 0.17519760131835935
  0.7600000000000001: 0.17006234259859432

|#
(newline)
(displayln "Optimal value of p")
(show "argmax2" (* 1.0 (/ (argmax2 (for/list ([p (range 0 1 0.001)]) (pascal_pdf 7 p 10))) 1000)))
(show "argmax2-val" (argmax2-val (for/list ([p (range 0 1 0.001)]) (pascal_pdf 7 p 10))))
(newline)
(for ([p (range 0.65 0.76 0.01)])
  (show p (pascal_pdf 7 p 10))
  )

