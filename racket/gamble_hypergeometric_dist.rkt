#| 

  Hypergeometric dist in Racket Gamble.

  https://en.wikipedia.org/wiki/Hypergeometric_distribution
  """
  (T)he probability of k successes (random draws for which the object 
  drawn has a specified feature) in n draws, without replacement, from 
  a finite population of size N that contains exactly K objects with 
  that feature, wherein each draw is either a success or a failure. 
  In contrast, the binomial distribution describes the probability of 
  k successes in n draws with replacement. 
  """

  Cf https://github.com/distributions-io/hypergeometric-random/blob/master/lib/number.js


  This is a port of my WebPPL model hypergeometric_dist.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

#|
  Testing pdf, cdf, and quantiles:

  (hypergeometric_pdf 50 5 10 4 : 30/7567 
  (hypergeometric_cdf 50 5 10 4 : 75661/75670 
  (hypergeometric_quantile 50 5 10 0.95 : 2)

|#
(let ((K 5)  ;; total green marbles: 4 drawn + 1 not drawn
      (N 50) ;; total marbles: 5 green + 45 red marbles
      (k 4)  ;; drawn 4 green_marbles
      ; (k 5) ;; drawn 5 green_marbles
      (n 10))   ;; total drawn green + red marbles
  (show2
   "\n(hypergeometric_pdf" N K n k ":"  (hypergeometric_pdf N K n k)
   "\n(hypergeometric_cdf" N K n k ":"  (hypergeometric_cdf N K n k)
  "\n(hypergeometric_quantile" N K n 0.95 ":" (hypergeometric_quantile N K n 0.95)
   ; (hypergeometric_quantile N K n 0.99)
   ; (hypergeometric_quantile N K n 0.999)
   )
  )
(newline)


#|
   Example from
   https://en.wikipedia.org/wiki/Hypergeometric_distribution#Working_example
   """
   Now, assume (for example) that there are 5 green and 45 red marbles in 
   the urn. Standing next to the urn, you close your eyes and draw 10 marbles 
   without replacement. What is the probability that exactly 4 of the 10 
   are green? 
   
   The probability of drawing exactly 4 marbels is: ~0.00396458305801506542
   The probability of drawing exactly 5 marbels is: ~0.00011893749174045196
   """
   
   This model (method:enumerate) give the following result:
   The probability of drawing exactly 4 marbels is: 30/7567 (0.003964583058015066)
   The probability of drawing exactly 5 marbels is: 9/75670 (0.00011893749174045196

   Which is quite exact...

   The count version, i.e. how many success objects we drew:

   4 marbles:
   1: 45695/105938 (0.43133719722856767)
   0: 82251/264845 (0.3105627820045687)
   2: 11115/52969 (0.20983971757065453)
   3: 2340/52969 (0.04417678264645358)
   4: 30/7567 (0.003964583058015066)
   5: 9/75670 (0.00011893749174045196)
   mean: 1 (1.0)

   5 marbles:
   1: 45695/105938 (0.43133719722856767)
   0: 82251/264845 (0.3105627820045687)
   2: 11115/52969 (0.20983971757065453)
   3: 2340/52969 (0.04417678264645358)
   4: 30/7567 (0.003964583058015066)
   5: 9/75670 (0.00011893749174045196)
   mean: 1 (1.0)

|#
(define (model1)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
    ;; total: 5 green and 45 red marbles
    ;; drawn: 4 green marbles, 6 red marbles
    (define K 5)  ;; total green marbles: 4 drawn + 1 not drawn
    (define N 50) ;; total marbles: 5 green + 45 red marbles
    
    (define k 4)    ;; drawn 4 green_marbles
    ; (define k 5) ;; drawn 5 green_marbles
    
    (define n 10)   ;; total drawn green + red marbles
    
    (define g (hypergeometric k N K n))
    (define c (hypergeometricCount k N K n)) ;; Count version
    
    (list g
          c
          )
    
    )
  )


(displayln "\nModel 1")
(show-marginals (model1)
                (list "g"
                      "c"
                      )
                #:num-samples 10000
                ; #:truncate-output 1
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:credible-interval2 0.84
                ; #:show-histogram? #t
                ;#:show-percentiles? #t
                )


#|
  Generate 20 examples from hypergeometricCount:

  (define : g
  (1 0 1 0 1 0 1 1 0 2 2 2 1 1 0 1 3 1 3 0): 0.0009999999999999994
  (2 1 3 1 1 1 2 1 2 2 2 2 1 0 3 0 0 0 2 0): 0.0009999999999999994
  (0 1 2 1 1 2 0 1 2 4 0 0 0 0 2 1 1 0 0 2): 0.0009999999999999994
  (0 0 0 1 0 0 0 0 0 0 1 3 0 0 2 3 2 3 1 3): 0.0009999999999999994
  (2 2 2 1 0 1 0 0 0 0 2 1 1 0 0 2 3 0 1 2): 0.0009999999999999994
  ...
  (3 2 0 1 2 1 1 1 0 0 2 0 0 0 0 0 2 2 0 1): 0.0009999999999999994
  (2 0 1 2 1 1 1 2 1 2 1 1 1 1 2 3 0 0 0 2): 0.0009999999999999994
  (1 1 3 3 2 2 0 1 1 2 1 0 2 0 1 1 2 2 0 0): 0.0009999999999999994
  (1 2 0 0 1 1 1 1 3 1 1 1 0 1 2 1 3 2 0 1): 0.0009999999999999994
  (1 0 1 0 1 0 1 1 0 0 0 2 0 1 0 1 1 1 2 1): 0.0009999999999999994

|#

; Simpler way to generate one instance
; (repeat (lambda () (hypergeometricCount 4 50 5 10)) 20)

(define (model2)
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
    ;; total: 5 green and 45 red marbles
    ;; drawn: 4 green marbles, 6 red marbles
    (define K 5)  ;; total green marbles: 4 drawn + 1 not drawn
    (define N 50) ;; total marbles: 5 green + 45 red marbles
    
    (define k 4)    ;; drawn 4 green_marbles
    ; (define k 5) ;; drawn 5 green_marbles
    
    (define n 10)   ;; total drawn green + red marbles
    
    ; (define g (hypergeometric k N K n))
    (define (draw i)  (hypergeometricCount k N K n))
    
    (list (for/list ((i 20)) (draw i))
          )
    
    )
  )


(show-marginals (model2)
                (list "g"
                      )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:credible-interval2 0.84
                ; #:show-histogram? #t
                ;#:show-percentiles? #t
                )


#|
  Trying to infer the parameters: K, k, N, and n.
  k should be 4 and n be 10
  It does not do a good job...

var : k
2: 0.0655367415678504
7: 0.058632808563432316
16: 0.057745891732026856
6: 0.05748009772743294
9: 0.05375208847138972
...
14: 0.04489215088629357
11: 0.044474775981540295
5: 0.044229133611085165
3: 0.04367163848862279
8: 0.03640648890485027
mean: 10.377438138250783
Credible interval (0.84): 1..17

var : n
5: 0.07045170533067573
4: 0.0700052625044441
8: 0.0691075240588469
12: 0.06905968082311013
2: 0.06875757021599757
...
21: 0.009284470188794275
22: 0.00802192192734976
23: 0.006025425291934482
24: 0.004833451302310613
25: 0.003074477915721445
mean: 8.84561611809842
Credible interval (0.84): 1..13



|#
(define (model3)
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ;; Data from model2
   (define data '(2 1 1 0 1 1 0 0 2 0 0 0 0 2 1 1 0 2 2 1))
    
   ;; total: 5 green and 45 red marbles
   ;; drawn: 4 green marbles, 6 red marbles
    
   (define K 5) ;; total green marbles: 4 drawn + 1 not drawn
   ;; (define K (add1 (+random-integer 20)); 
    
   (define N 50) ;; total marbles: 5 green + 45 red marbles
    
   ;; (define k = 4; ;; drawn green_marbles
   (define k (add1 (random-integer 20)))
   ;; (define k (poisson 5))
        
   ; (define n 10) ;; total drawn green + red marbles
   (define n (add1 (random-integer 25)))
    
   ;; (define g = hypergeometric(k,N,K,n);
   (defmem (draw i)
     (hypergeometricCount k N K n))
  
   (for ([i (length data)])
     ; (observe/fail (= (draw i) (list-ref data i))))
     (observe-sample (normal-dist (draw i) 3) (list-ref data i)))
  
   (list k  ;; should be 4
         n  ;; should be 10
         )
    )
  )

#|
(show-marginals (model3)
                (list "k"
                      "n"
                      )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:credible-interval2 0.84
                ; #:show-histogram? #t
                ;#:show-percentiles? #t
                )
|#
