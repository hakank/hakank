#| 

  A/B testing, given lists of successes for A and B in Racket Gamble.

  This is a port of my WebPPL model ab_testing2.wppl

var : pA
0.08440911974428911: 0.9529300760567376
0.027431499590714135: 0.04703912102863514
0.1092925491180931: 2.7156545923772895e-5
0.11868886099814238: 2.9735873302969894e-6
0.12200461422838628: 4.4747153509588374e-7
...
0.5479810698539045: 1.2038245233265694e-202
0.07966284794337546: 1.0220748173181177e-202
0.17362621158718275: 2.1681670810890613e-203
0.38634135056481544: 2.1382318353935516e-206
0.5425691924148436: 2.1382318353935516e-206
mean: 0.081729740490593
Min: 0.040173448716625766 Mean: 0.0669591550238187 Max: 0.06826812803873057 Variance: 3.487104608197959e-5 Stddev: 0.005905171130626071
Credible interval (0.94): 0.06826812803873057..0.06826812803873057

var : pB
0.0409006963438234: 0.9529300760567376
0.02573967842091004: 0.04703912102863514
0.019991807326414635: 2.7156545923772895e-5
0.03819105103761857: 2.9735873302969894e-6
0.04059084702858627: 4.4747153509588374e-7
...
0.12706749647595328: 1.2038245233265694e-202
0.536618672263459: 1.0220748173181177e-202
0.5075321668271665: 2.1681670810890613e-203
0.37051489959854145: 2.1382318353935516e-206
0.1520782033444885: 2.1382318353935516e-206
mean: 0.04018696500663955
Min: 0.033768445478025454 Mean: 0.034455067616053554 Max: 0.06384676000631453 Variance: 9.803902700436303e-6 Stddev: 0.0031311184424157932
Credible interval (0.94): 0.033768445478025454..0.033768445478025454

var : diff
0.04350842340046571: 0.9529300760567376
0.001691821169804094: 0.04703912102863514
0.08930074179167846: 2.7156545923772895e-5
0.0804978099605238: 2.9735873302969894e-6
0.08141376719980001: 4.4747153509588374e-7
...
0.4209135733779512: 1.2038245233265694e-202
-0.4569558243200836: 1.0220748173181177e-202
-0.3339059552399838: 2.1681670810890613e-203
0.39049098907035507: 2.1382318353935516e-206
0.015826450966273997: 2.1382318353935516e-206
mean: 0.041542775483953465
Min: -0.012196398063020163 Mean: 0.03250408740776362 Max: 0.03449968256070512 Variance: 8.076785423721454e-5 Stddev: 0.00898709375923132
Credible interval (0.94): 0.03449968256070512..0.03449968256070512

var : a>b
#t: 0.9999999880665817
#f: 1.1933418297037596e-8
mean: 0.9999999880665817
Min: 0 Mean: 0.953 Max: 1 Variance: 0.044791 Stddev: 0.21163884331568247
Credible interval (0.94): 1..1

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


;; Generated by ./gener_data.pl
;; True prob 0.05 
;; random_binomial(750,1,0.05);   
(define *as* '(0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 
                 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 
                 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0))


;  Generated by ./gener_data.pl
; True prob = 0.04
; random_binomial(750,1,0.04);
(define *bs* '(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 1 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0))
 

(define (ab-testing2)
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; slower than importance-sampler
   
   (define pA (beta 1 1)) ; true prob: 0.05
   (define pB (beta 1 1)) ; true prob: 0.04

   (define a (mem (lambda (i) (bernoulli-dist pA))))
   (define b (mem (lambda (i) (bernoulli-dist pB))))
   ;; (define a (mem (lambda (i) (bernoulli pA))))
   ;; (define b (mem (lambda (i) (bernoulli pB))))
   
   (define diff (- pA pB))
   (define a>b (> diff 0))

   (for ([i (range (length *as*))])
     (observe-sample (a i) (list-ref *as* i))
     (observe-sample (b i) (list-ref *bs* i))
     ;; (observe/fail (= (a i) (list-ref *as* i)))
     ;; (observe/fail (= (b i) (list-ref *bs* i)))
     )
   
   (list pA pB diff a>b)
   )
  )

(show-marginals (ab-testing2)
                '("pA" "pB" "diff" "a>b")
                #:truncate-output 5
                #:show-stats? #t
                #:credible-interval 0.94
                )
