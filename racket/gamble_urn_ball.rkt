#| 

  Urn ball in Racket Gamble.

  From a BLOG model in
  Yi Wu, Lei Li, Stuart Russell, Rastislav Bodik
  "Swift: Compiled Inference for Probabilistic Programming Languages"  
  Page 2

var : drawn4
0: 0.7536000000000033
1: 0.11530000000000054
2: 0.048800000000000225
3: 0.02280000000000011
4: 0.014300000000000068
5: 0.010900000000000049
6: 0.00690000000000003
8: 0.004700000000000018
9: 0.0041000000000000186
7: 0.003900000000000016
10: 0.0035000000000000144
12: 0.0022000000000000097
11: 0.0019000000000000087
13: 0.001700000000000008
14: 0.001700000000000008
15: 0.001500000000000007
16: 0.0010000000000000044
17: 0.0005000000000000023
19: 0.0004000000000000018
18: 0.00030000000000000133
mean: 0.7244000000000034

var : color4
Green: 0.8336000000000027
Blue: 0.16640000000000044

var : numBalls
1: 0.6401000000000029
2: 0.1331000000000006
3: 0.06300000000000028
4: 0.03690000000000016
5: 0.02390000000000011
6: 0.016800000000000075
7: 0.015100000000000068
8: 0.011200000000000052
9: 0.009500000000000041
10: 0.005900000000000026
13: 0.005800000000000026
11: 0.005500000000000023
12: 0.005400000000000023
15: 0.005100000000000022
16: 0.004400000000000019
19: 0.004000000000000017
14: 0.003900000000000017
17: 0.0039000000000000167
18: 0.0034000000000000146
20: 0.0031000000000000133
mean: 2.470700000000011

var : D
#(#f #f #f #f #f): 1.0000000000000016


  This is a port of my WebPPL model urn_ball.wpppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (; enumerate ; too slow
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define D (make-vector 5 #f))  ; 5 draws: D(0),D(1),D(2),D(3), and D(4) (the unobserved)
  
   (define numBalls (add1 (random-integer 20)))
      
   (define color (mem (lambda (b) 
        (categorical-vw2 (vector 0.9 0.1) (vector "Blue" "Green")))))
    
   (define (drawn d) (random-integer numBalls))


    ;; Original problem
   (observe/fail (eq? (color (drawn (vector-ref D 0))) "Green"))
   (observe/fail (eq? (color (drawn (vector-ref D 1))) "Green"))
   (observe/fail (eq? (color (drawn (vector-ref D 2))) "Green"))
   (observe/fail (eq? (color (drawn (vector-ref D 3))) "Green"))

   ;; Another problem
   #|
   (observe/fail (eq? (color (drawn (vector-ref D 0))) "Blue"))
   (observe/fail (eq? (color (drawn (vector-ref D 1))) "Blue"))
   (observe/fail (eq? (color (drawn (vector-ref D 2))) "Blue"))
   (observe/fail (eq? (color (drawn (vector-ref D 3))) "Blue"))
   |#
   
   (list (drawn (vector-ref D 4))
         (color (drawn (vector-ref D 4)))
         numBalls
         ; D
         )
   
   )
  )

(show-marginals (model)
                (list "drawn4"
                      "color4"
                      "numBalls"
                      ; "D"
                      )
                #:num-samples 1000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:show-stats? #t
                )

