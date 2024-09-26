#| 

  The blind archer in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/1006
  """
  A very sharp, consistently skillful blind archer aimed for the 
  center of a circular board and shot 2 arrows. He is expected to 
  hit the aim, but doesn't hit it for sure. The archer is told that 
  his first shot is better than second. He tried one more shot. What is 
  the probability that this 3rd shot is the best shot among 3? (ie, 
  Probability that 3rd arrow lands closer to center than his first two shots?)

  Answer: 1/3

  Solution: ... Notice that if there were (N-1) arrows, with first being 
            better than rest, and then he shoots Nth arrow. The probability 
            that Nth shot is best is 1/N.
  """

  * Some experiments with different precision (standard deviation) using model 1:

  var : arrow1
  mean: -0.043485939470135515
  Credible interval (0.84): -0.8001163026467706..0.9068272710301848

  var : arrow2
  mean: 0.005722429652225795
  Credible interval (0.84): -1.8163817962350874..1.6305995153506603

  var : arrow3
  mean: -0.025572072838159446
  Credible interval (0.84): -1.3736023141049962..1.4647247481990098

  var : p
  mean: 0.34160000000001634
  Credible interval (0.84): 0..1

  * With sigma 0.1 (better precision of the archer)

  var : arrow1
  mean: -0.00016643753411346032
  Credible interval (0.84): -0.08725569623555197..0.08337410899381312

  var : arrow2
  mean: -0.0003351402637321025
  Credible interval (0.84): -0.17943250550445888..0.1671398293587143

  var : arrow3
  mean: -0.0003424778970128923
  Credible interval (0.84): -0.13983782443404313..0.1377478458724966

  var : p
  mean: 0.33929000000009435
  Credible interval (0.84): 0..1


  * With sigma 10 (less precision of the archer)
  var : arrow1
  mean: 0.07504806417648924
  Credible interval (0.84): -8.60273103482601..7.565757913376288

  var : arrow2
  mean: 0.05234513731503737
  Credible interval (0.84): -16.359949942422848..15.945766179577246

  var : arrow3
  mean: 0.7236038748153689
  Credible interval (0.84): -11.990043268172155..16.218505927188694

  var : p
  mean: 0.31000000000001476
  Credible interval (0.84): 0..1

  * With sigma 100 (quite bad precision of the archer)
  var : arrow1
  mean: -3.772779892498051
  Credible interval (0.84): -78.97231358455838..86.8239647719908

  var : arrow2
  mean: 8.802213845978804
  Credible interval (0.84): -156.31969534190262..169.3425679146362

  var : arrow3
  mean: 0.521847439529012
  Credible interval (0.84): -146.95125177976576..130.87455596620498

  var : p
  mean: 0.32630000000001586
  Credible interval (0.84): 0..1

  * Model 2 supports different number of arrows 
   estimated value of p = 1/num-arrows

  - 3 arrows

  var : best
  2: 0.33788
  0: 0.33381
  1: 0.32831
  mean: 1.00407

  var : p
  #f: 0.66212
  #t: 0.33788
  mean: 0.33788

  - 7 arrows (expected p = 1/7= 0.14286

  var : best
  5: 0.14928
  2: 0.14625
  0: 0.14415
  1: 0.14306
  6: 0.14243
  4: 0.14139
  ...
  3: 0.13344
  mean: 3.00242

  var : p
  #f: 0.8575699999999999
  #t: 0.14243
  mean: 0.14243

  * Model 3: Using integers (enumerate), 3 arrows (max-value 10)
  var : best
  0: 77/200 (0.385)
  1: 33/100 (0.33)
  2: 57/200 (0.285)
  mean: 9/10 (0.9)

  var : p
  #f: 143/200 (0.715)
  #t: 57/200 (0.285)
  mean: 57/200 (0.285)

   

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
   ; importance-sampler
   mh-sampler

   (define sigma 1)
   
   (define arrow1 (normal 0 sigma))
   (define arrow2 (normal 0 sigma))
   (define arrow3 (normal 0 sigma))

   (observe/fail (< (abs arrow1) (abs arrow2)))

   (define p (< (abs arrow3) (abs arrow1)))

   (list arrow1
         arrow2
         arrow3
         p)
   

   )
)

(displayln "\nModel 1")
(show-marginals (model)
                (list  "arrow1"
                       "arrow2"
                       "arrow3"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


(define (model2)
  (; enumerate
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define sigma 1)

   (define num-arrows 7)

   (define (arrow a) (normal 0 sigma))
   
   (for ([i (range 1 num-arrows)])
     (observe/fail (< (abs (arrow 0)) (abs (arrow i)))))

   (define all-arrows (for/list ([i num-arrows]) (arrow i)))
   (define best (argmin2 all-arrows))
   
   (define p (= (sub1 num-arrows) best))

   (list best
         p)
   

   )
)

(displayln "\nModel 2")
(show-marginals (model2)
                (list  "best"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


;
; Using integers instead
;
(define (model3)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define num-arrows 3)

   (define (arrow a) (random-integer 10))
   
   (for ([i (range 1 num-arrows)])
     (observe/fail (< (abs (arrow 0)) (abs (arrow i)))))

   (define all-arrows (for/list ([i num-arrows]) (arrow i)))
   (define best (argmin2 all-arrows))
   
   (define p (= (sub1 num-arrows) best))

   (list all-arrows
         best
         p)
   

   )
)

(displayln "\nModel 3")
(show-marginals (model3)
                (list  "all-arrows"
                       "best"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


