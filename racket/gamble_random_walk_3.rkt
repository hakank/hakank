#| 

  Random walk in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/215
  """
  You are initially located at the origin on the x-axis. You start a 
  random walk with an equal probability of moving left or right, one 
  step at a time. What is the probability that you will reach point a
  before reaching point -b.

  Assume a and b are natural numbers. a > 1; b > 1.

  Answer: (b / (a + b))
  ...
  [I]t takes on average a*b steps to end the game.
  """
 
  Some experiments:

(a 5 b 3)
var : num-steps
5: 0.12468000000000004
3: 0.12463000000000005
7: 0.10953000000000004
9: 0.09292000000000003
11: 0.08154000000000003
...
105: 1.0000000000000004e-5
115: 1.0000000000000004e-5
117: 1.0000000000000004e-5
125: 1.0000000000000004e-5
127: 1.0000000000000004e-5
mean: 14.969360000000004

var : num-steps-theoretical
15: 0.9999999999999999
mean: 14.999999999999998

var : last-value
-3: 0.6240099999999998
5: 0.37599000000000016
mean: 0.00792000000000126

var : last-value-theretical
3/8: 0.9999999999999999
mean: 0.37499999999999994


(a 2 b 3)
var : num-steps
2: 0.24952000000000002
3: 0.12496000000000002
4: 0.12423000000000001
5: 0.09529000000000001
6: 0.07660000000000002
...
44: 1.0000000000000003e-5
46: 1.0000000000000003e-5
47: 1.0000000000000003e-5
48: 1.0000000000000003e-5
51: 1.0000000000000003e-5
mean: 6.0111799999999995

var : num-steps-theoretical
6: 1.0
mean: 6.0

var : last-value
2: 0.59598
-3: 0.40402
mean: -0.020100000000000007

var : last-value-theretical
3/5: 1.0
mean: 0.6

(a 5 b 8)
var : num-steps
9: 0.04081000000000006
7: 0.038350000000000065
11: 0.03761000000000005
13: 0.033790000000000056
5: 0.031180000000000048
...
334: 1.0000000000000016e-5
339: 1.0000000000000016e-5
369: 1.0000000000000016e-5
382: 1.0000000000000016e-5
473: 1.0000000000000016e-5
mean: 40.081450000000046

var : num-steps-theoretical
40: 0.9999999999999999
mean: 39.99999999999999

var : last-value
5: 0.6136900000000005
-8: 0.3863100000000006
mean: -0.022030000000002214

var : last-value-theretical
8/13: 0.9999999999999999
mean: 0.6153846153846153




  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (define a (add1 (random 10)))
  (define b (add1 (random 10)))
  (show2 "a" a "b" b)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define (f lst)
     (if (or (member a lst) (member (- b) lst))
         lst
         (f (append lst (list (+ (last lst)
                                 (uniform-draw '(-1 1))))))
         )
     )

   (define lst (f '(0)))
   (define num-steps (- (length lst) 1)) ; Don't count the first (0)
   (define last-value (last lst))
   
   (list num-steps
         (* a b)      ; theoretical num-steps
         last-value
         (/ b (+ a b) ; theoretical for last-value
            )
         )
   
   )
)

(show-marginals (model)
                (list  "num-steps"
                       "num-steps-theoretical"
                       "last-value"
                       "last-value-theretical"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


