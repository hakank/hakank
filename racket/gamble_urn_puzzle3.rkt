#| 

  Urn puzzle in Racket.Gamble 

  From "Perplexing the Web, One Probability Puzzle at a Time" 
  https://www.quantamagazine.org/perplexing-the-web-one-probability-puzzle-at-a-time-20240829/?mc_cid=94caee8978
  (about Daniel Litt)
  """
  Puzzle 1

  You have 100 urns, each contain 99 ball. In 99 of the urns, one ball is red,
  and the rest are green. In the last, all 99 balls are red. You pick an urn 
  at random and draw a ball. It's red. Now throw it away. 
  The next ball you draw from the SAME urn is probably 
  * Red
  * Green
  * Equally likely
  """

  This prpblem was tweeted by Daniel Litt (Jan 29, 2024)
  https://x.com/littmath/status/1751971133723656585
  """
  ... 
  Red: 25.7%
  Green: 28.1%
  Equally likely: 32.3%
  Don’t know/see results: 13.9%
  """

  It's equally likely that ball2 is red or green.

  var : urn
  99: 1/2 (0.5)
  0: 1/198 (0.005050505050505051)
  1: 1/198 (0.005050505050505051)
  2: 1/198 (0.005050505050505051)
  3: 1/198 (0.005050505050505051)
  ...
  94: 1/198 (0.005050505050505051)
  95: 1/198 (0.005050505050505051)
  96: 1/198 (0.005050505050505051)
  97: 1/198 (0.005050505050505051)
  98: 1/198 (0.005050505050505051)
  mean: 74 (74.0)

  var : ball2
  green: 1/2 (0.5)
  red: 1/2 (0.5)

  Also, note that it's even chance that we drew from the last (100'th) urn with 
  the 99 red balls.

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

   (define num-urns 100)

   (define colors (vector "red" "green"))
   
   (define (urns i)
     (if (= i 99)
         (categorical-vw2 (vector 99/99  0/99) colors)  ; last urn
         (categorical-vw2 (vector  1/99 98/99) colors)  ; the rest of the urns
         ))

   (define urn (random-integer num-urns))

   (define ball1 (urns urn))

   ; You pick a ball. It's red.
   ;
   ; Without this important condition, the probabilities are
   ;   green: 49/50 (0.98)
   ;   red: 1/50 (0.02)
   (observe/fail (eq? ball1 "red"))
   
   ; Throw away the first ball and pick a new ball from the SAME urn
   (define ball2
     (cond [(= urn 99) "red"] ; There are only red balls in last urn.
           [(if (eq? ball1 "red")
                "green" ; We drew the only red from that urn so it must be green
                (categorical-vw2 (vector  1/98 97/98) colors))] ; remove the green ball and draw again
           ))

   (list urn
         ball2
         )

   )
)

(show-marginals (model)
                (list "urn"
                      "ball2"
                     )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


