#| 

  Urn puzzle in Racket.Gamble 

  From Daniel Litt  (Jan 28, 2024)
  https://x.com/littmath/status/1751648838501224790
  """
  You are given an urn containing 100 balls; n of them are red, and 100-n are green, 
  where n is chosen uniformly at random in [0, 100]. You take a random ball out of 
  the urn—it’s red—and discard it. The next ball you pick (out of the 99 remaining) is:
    More likely to be red: 22.6%
    More likely to be green: 37.1%
    Equally likely: 20.9%
    Don’t know/see results: 19.5%
  """

  Note: The "official" answer 
  (in https://www.quantamagazine.org/perplexing-the-web-one-probability-puzzle-at-a-time-20240829/?mc_cid=94caee8978) is that probability of the next ball red is 2/3, and blue 1/3.

  However, I got a slightly different answer by restricting to be at least one red ball.

  Using enumerate

  var : num-red
  100: 101/5150 (0.019611650485436893)
  99: 2/103 (0.019417475728155338)
  98: 99/5150 (0.019223300970873786)
  97: 49/2575 (0.019029126213592235)
  96: 97/5150 (0.01883495145631068)
  ...
  5: 3/2575 (0.0011650485436893205)
  4: 1/1030 (0.000970873786407767)
  3: 2/2575 (0.0007766990291262136)
  2: 3/5150 (0.0005825242718446602)
  1: 1/2575 (0.0003883495145631068)
  mean: 6868/103 (66.67961165048544)

  var : num-green
  0: 101/5150 (0.019611650485436893)
  1: 2/103 (0.019417475728155338)
  2: 99/5150 (0.019223300970873786)
  3: 49/2575 (0.019029126213592235)
  4: 97/5150 (0.01883495145631068)
  ...
  95: 3/2575 (0.0011650485436893205)
  96: 1/1030 (0.000970873786407767)
  97: 2/2575 (0.0007766990291262136)
  98: 3/5150 (0.0005825242718446602)
  99: 1/2575 (0.0003883495145631068)
  mean: 3432/103 (33.320388349514566)

  var : ball1
  0: 2/103 (0.019417475728155338)
  1: 2/103 (0.019417475728155338)
  2: 99/5150 (0.019223300970873786)
  3: 49/2575 (0.019029126213592235)
  4: 97/5150 (0.01883495145631068)
  ...
  96: 1/1030 (0.000970873786407767)
  97: 2/2575 (0.0007766990291262136)
  98: 3/5150 (0.0005825242718446602)
  99: 1/2575 (0.0003883495145631068)
  100: 1/5150 (0.0001941747572815534)
  mean: 3434/103 (33.33980582524272)

  var : color-ball1
  red: 1 (1.0)

  var : ball2
  100: 5149/515000 (0.009998058252427184)
  99: 1287/128750 (0.00999611650485437)
  98: 5147/515000 (0.009994174757281553)
  97: 2573/257500 (0.009992233009708739)
  96: 1029/103000 (0.009990291262135922)
  ...
  4: 5053/515000 (0.009811650485436894)
  3: 1263/128750 (0.009809708737864077)
  2: 5051/515000 (0.009807766990291263)
  0: 101/10300 (0.009805825242718447)
  1: 101/10300 (0.009805825242718447)
  mean: 129179/2575 (50.16660194174757)

  var : color-ball2
  red: 1717/2575 (0.6667961165048544)
  green: 858/2575 (0.3332038834951456)

  I.e. the probabiity is about 2/3 that the next ball is red, and about 1/3 that it's green.

  Note: 
  * without the constraint (num-red > 0), then the probabilities are exactly 2/3 and 1/3:
      red: 2/3 (0.6666666666666666)
      green: 1/3 (0.3333333333333333)
  
  See https://colab.research.google.com/drive/1vK8sl3ZecjBTjrcNcee6hmlHvrp3Zh4_?usp=sharing
  for another (Bayesian simulation) approach. It gives 2/3 and 1/3. 

  This problem was found via the Quanta Magazine article on Daniel Litt:
  "Perplexing the Web, One Probability Puzzle at a Time" by Erica Klarreich:
  https://www.quantamagazine.org/perplexing-the-web-one-probability-puzzle-at-a-time-20240829/?mc_cid=94caee8978

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define total 100)
   (define total1 (add1 total))
   (define num-red (random-integer total1)) ; number of of balls: 0..100

   ; There must be at least one red ball, i.e. the one we first select.
   ; Without this, the probabilities are exactly 2/3 red and 1/3 green,
   ; which is the official answer
   (observe/fail (> num-red 0))
   
   (define num-green (- total num-red))

   ; What color is the ball?
   (defmem (color ball) (if (<= ball num-red) "red" "green"))
   ; (define (color ball) (if (<= ball num-red) "red" "green"))   
   
   (define ball1 (random-integer total1))
   (define color-ball1 (color ball1))

   ;; The first ball is red
   (observe/fail (eq? color-ball1 "red"))
   
   (define ball2 (random-integer total1))
   (define color-ball2 (color ball2))   

   ; Discard first ball since it cannot be selected.
   (observe/fail (not (= ball2 ball1)))
   
   (list num-red
         num-green
         ball1
         color-ball1
         ball2
         color-ball2
         )
   
   

   )
)

(show-marginals (model)
                (list  "num-red"
                       "num-green"
                       "ball1"
                       "color-ball1"
                       "ball2"
                       "color-ball2"
                     )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


