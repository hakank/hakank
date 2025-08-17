#| 

  Three biased coins in Racket/Gamble 

  From Pascal Bercker: "The problem of three biased coins - A Bayesian network solution."
  https://medium.com%2F@medium.com/@pbercker/the-problem-of-three-biased-coins-a-bayesian-network-solution-310cbfe70572
  """
  [Coin 1     Coin 2     Coin 3
   p(H):0.4   p(H):0.6      p(H):0.8

  [S]uppose you have 3 biased coins with the given biased probabilities for landing heads. 
  Given that one was randomly picked and tossed 5 times landing {H, H, H, T, T}, which coin was 
  most likely picked? 

  [References this YouTube video by Serrabo.Academi "The Beta distribution in 12 minutes!"
   https://www.youtube.com/watch?v=juF3r12nM5A
   that includes the three biased coins example.
  ]
  """
 

  The exact probabilities for each coin bias in target1 (H, H, H, T, T):
  variable : p
  0.6: 0.44262295081967223
  0.4: 0.29508196721311475
  0.8: 0.262295081967213
  mean: 0.5934426229508196

  For the target (1 1 1 1 1 1 1 0 0 0) (mentioned in the YouTube video), the
  probabilities are then:
  variable : p
  0.6: 0.4686093850439257
  0.8: 0.4388257981572742
  0.4: 0.09256481679880006
  mean: 0.6692521962716947


  For target 3 (generated "manually" random), 19.3s
  variable : p
  0.4: 0.5968946799154932
  0.6: 0.397929786610329
  0.8: 0.005175533474177944
  mean: 0.48165617071173705

  See below for some alternative models
  * model2: Using (beta a b) for recovering a and b
  * model3: Using binomial distribution.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define target1 '(1 1 1 0 0))
(define target2 '(1 1 1 1 1 1 1 0 0 0))
; A third sequence (by manual randomness)
(define target3 '(1 0 1 1 1 0 0 1 0 0 0 1 0 1 1 0 0 0 1))

(define target target1)
; (define target target2)
; (define target target3)

(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n (length target))

   (define p (uniform-draw '(0.4 0.6 0.8)))
   (define coins (for/list ([i (range n)]) (bernoulli p)))

   (observe/fail (equal? coins target))

   (list p
         ; coins
         )
   
   )
)

(show-marginals (model1)
                (list  "p"
                       "coins"
                       )
                #:num-samples 1000
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
   
  Using p = (beta a b) distribution, recovering the a and b parameters,
  but still using the (bernoulli p) as the list generator.

  Here we force a and b to be integers, but do not force p to be one
  of 0.4, 0.6, and 0.8.

  For target1:
variable : p
0.4267681936067539: 0.00010000000000000938
0.40040016855926597: 0.00010000000000000938
0.401540361111451: 0.00010000000000000938
0.6390007379370081: 0.00010000000000000938
0.6262793393020133: 0.00010000000000000938
...
0.6186658679534772: 0.00010000000000000938
0.6060415636597435: 0.00010000000000000938
0.6607340683486539: 0.00010000000000000938
0.9182220856450066: 0.00010000000000000938
0.5656694513672902: 0.00010000000000000938
mean: 0.5563683745278774

variable : a
9: 0.1285000000000085
8: 0.12590000000000806
10: 0.12060000000000777
7: 0.11840000000000771
5: 0.11030000000000748
6: 0.11030000000000748
...
4: 0.09970000000000717
3: 0.08770000000000683
2: 0.06210000000000607
1: 0.03650000000000356
mean: 6.23440000000042

variable : b
5: 0.1179000000000077
4: 0.11670000000000766
3: 0.1146000000000076
7: 0.10590000000000735
6: 0.10560000000000734
8: 0.09860000000000714
...
2: 0.09760000000000711
9: 0.08540000000000676
10: 0.08490000000000675
1: 0.0728000000000064
mean: 5.449400000000393

  For target2:

0.8771813631739488: 0.0009999999999999994
0.868425426170968: 0.0009999999999999994
0.7748597108120572: 0.0009999999999999994
0.48222062014070705: 0.0009999999999999994
0.8120297059194432: 0.0009999999999999994
...
0.6499626927272271: 0.0009999999999999994
0.7130484784908275: 0.0009999999999999994
0.7219388200896056: 0.0009999999999999994
0.703611231754229: 0.0009999999999999994
0.4819232957282519: 0.0009999999999999994
mean: 0.641926779125467

variable : a
20: 0.08600000000000005
18: 0.08200000000000005
19: 0.07800000000000004
16: 0.07600000000000004
17: 0.07100000000000004
...
5: 0.028
4: 0.019999999999999993
2: 0.009999999999999993
3: 0.007999999999999995
1: 0.005999999999999996
mean: 13.341000000000005

variable : b
6: 0.07500000000000004
5: 0.07400000000000004
3: 0.07200000000000004
8: 0.07200000000000004
4: 0.06900000000000003
...
1: 0.031000000000000003
16: 0.031000000000000003
18: 0.023999999999999997
19: 0.023999999999999997
20: 0.018999999999999993
mean: 8.889000000000001


|#
(define (model2)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n (length target))

   (define a (add1 (random-integer (* 2 n))))
   (define b (add1 (random-integer (* 2 n))))
   (define p (beta a b))
   
   (define coins (for/list ([i (range n)]) (bernoulli p)))
   
   (observe/fail (equal? coins target))
   ; Sorting (same probabilities)
   ; (observe/fail (equal? (sort coins <) (sort target <)))

   (list p
         a
         b
         ; coins
         )
   
   )
)

#|
(show-marginals (model2)
                (list  "p"
                       "a"
                       "b"                       
                       "coins"
                       )
                #:num-samples 1000
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

|#


#|
  Using binomial distribution.
  The probability distribution is - unsurprisingly - the same as
  in the first model.

  * target 1
  variable : p
  0.6: 0.4426229508196724
  0.4: 0.29508196721311486
  0.8: 0.2622950819672127
  mean: 0.5934426229508195

 * target 2:
  variable : p
  0.6: 0.46860938504392474
  0.8: 0.4388257981572746
  0.4: 0.09256481679880064
  mean: 0.6692521962716947

  * target 3 (this takes 25.3s so it's slower than model1)
  variable : p
  0.4: 0.596894679915532
  0.6: 0.39792978661037615
  0.8: 0.005175533474091907
  mean: 0.48165617071171196




|#
(define (model3)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n (length target))
   (define target-num-heads (sum target))

   (define p (uniform-draw '(0.4 0.6 0.8)))
   (define coins (for/list ([i (range n)]) (bernoulli p)))
   (define num-heads (sum coins))
   
   (observe-sample (binomial-dist n p) target-num-heads)
   
   (list p
         ; coins
         )
   
   )
)

#|
(show-marginals (model3)
                (list  "p"
                       "coins"
                       )
                #:num-samples 1000
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

|#
