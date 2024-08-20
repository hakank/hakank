#| 

  Gaussian mixture model in Racket Gamble.

  From
  "CAV 2020 Tutorial: Probabilistic Programming: A Guide for Verificationists"
  https://www.youtube.com/watch?v=yz5uUf_03Ik&t=2657s
  Around @23:30

var : pr
0.5465150209318671: 0.002003941564063752
0.0310846368003647: 0.002003941564063752
0.7452782131773114: 0.002003941564063752
0.4859592302421853: 0.002003941564063752
...
0.6688103645836366: 6.618669100779622e-8
0.8698652060544033: 6.618669100779622e-8
0.9348820286000759: 6.618669100779622e-8
0.8939307294640672: 6.618669100779622e-8
mean: 0.33423639407964695
Histogram:
    0: 2  
0.048: 112
0.097: 107
0.145: 106
0.193: 60 
0.242: 62 
 0.29: 94 
0.339: 55 
0.387: 54 
0.435: 73 
0.484: 38 
0.532: 52 
 0.58: 35 
0.629: 28 
0.677: 18 
0.725: 23 
0.774: 33 
0.822: 12 
 0.87: 17 
0.919: 9  

var : p
#f: 0.9999668404678248
#t: 3.315953219490571e-5
mean: 3.315953219490571e-5
Histogram:
#f: 1001

var : x
14.607472793706687: 0.002003941564063752
15.143739951836094: 0.002003941564063752
14.343414341903669: 0.002003941564063752
15.120333668998237: 0.002003941564063752
...
10.532122527740686: 6.618669100779622e-8
9.59699508179064: 6.618669100779622e-8
9.747204087969864: 6.618669100779622e-8
10.525191547213463: 6.618669100779622e-8
mean: 13.99672844881934
Histogram:
12.308: 2  
12.476: 0  
12.644: 4  
12.813: 1  
12.981: 1  
13.149: 31 
13.318: 43 
13.486: 79 
13.654: 104
13.823: 104
13.991: 139
14.159: 145
14.328: 102
14.496: 71 
14.664: 71 
14.833: 47 
15.001: 34 
15.169: 8  
15.338: 5  
15.506: 7  

  This is a port of my WebPPL model gaussian_mixture_model2.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (gaussian-mixture-problem-2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define pr (uniform 0 1))
   (define p (flip pr))
    
   (define x (if p
                 (normal-dist 10.0 1.0)
                 (normal-dist 14.0 0.5)))

   (observe-sample x 14.5)
   ;; (observe-sample x 8.5)
   ;; (observe-sample x 12.5)   

   (list pr p (sample x))

   )
  )

(show-marginals (gaussian-mixture-problem-2)
                (list "pr"
                      "p"
                      "x"
                      )
                #:num-samples 1000
                #:truncate-output 4
                #:show-histogram? #t
                )

