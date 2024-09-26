#| 

  One rigged coin in Racket.Gamble 

  https://x.com/littmath/status/1827447490683003234
  """
  You have 10000 coins. 9999 of them are fair; one is rigged so that it always 
  lands on heads. You choose a coin at random and flip it 10 times; it’s heads 
  all ten times. The coin is probably
  - fair
  - rigged
  - equaly likely
  - can't tell/see results
  """

  For different n tosses with n heads

  n: 10
  var : pick_coin
  fair_coin: 9999/11023 (0.9071033294021591)
  biased_coin: 1024/11023 (0.09289667059784087)

  n: 15
  var : pick_coin
  biased_coin: 32768/42767 (0.7661982369584025)
  fair_coin: 9999/42767 (0.2338017630415975)

  n: 20
  var : pick_coin
  biased_coin: 1048576/1058575 (0.9905542828802871)
  fair_coin: 9999/1058575 (0.009445717119712822)

  n: 25
  var : pick_coin
  biased_coin: 33554432/33564431 (0.9997020953520708)
  fair_coin: 9999/33564431 (0.000297904647929232)

  n: 50
  var : pick_coin
  biased_coin: 1125899906842624/1125899906852623 (0.9999999999911191)
  fair_coin: 9999/1125899906852623 (8.880896018502682e-12)

  n: 100
  var : pick_coin
  biased_coin: 1267650600228229401496703205376/1267650600228229401496703215375 (1.0)
  fair_coin: 9999/1267650600228229401496703215375 (7.887820191304897e-27)

  (This run takes about 4s.)


  Calculation (for n=10) from
  https://x.com/RRichtsfeld/status/1827476479048880528
  """
  Expected fair all heads: 9999/2^10
  Expected rigged all heads: 1
  Expected total all heads: 9999/2^10+1
  Conditional probability of fair all heads: (9999/2^10)/(9999/2^10+1)≈90.71%
  Conditional probability of rigged all heads: 1/(9999/2^10+1)≈9.29%
  """


  Cf my WebPPL model one_rigged_coin.wppl (which is actually slower)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model n)
  (enumerate 
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler ; #:transition (slice)

   ; (define n 15)
   
   (define (biased_coin i) "head")
   
   (define (fair_coin i)
     (categorical-vw2 (vector 1/2 1/2) (vector "head" "tail")))

   ; Throw the coin n times ...   
   (define pick_coin (if (flip 9999/10000) "fair_coin" "biased_coin"))

   (define (a i)
     (if (eq? pick_coin "fair_coin")
         (fair_coin i)
         (biased_coin i)))

   ; And observe n heads
   (for ([i n])
     (observe/fail (eq? (a i) "head")))
     
   (list pick_coin
         )
   
   )
  )

; Number of coin tosses with n heads
(for ([n '(10 15 20 25 50 100)])
  (show "n" n)
  (show-marginals (model n)
                  (list  "pick_coin"
                         )
                  #:num-samples 10000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.94
                  ; #:credible-interval2 0.94                
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )
  )
