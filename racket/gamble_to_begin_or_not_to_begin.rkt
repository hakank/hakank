#| 

  To begin of not to begin in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/1002
  """
  A & B are alternately picking balls from a bag without replacement. 
  The bag has k black balls and 1 red ball. Winner is the one who picks the 
  red ball. Who is more likely to win, the on who starts first, or second?

  Answer: Begin!
  Solution: ... Hence, doesnt matter who starts first when k is odd. 
            The first player has higher chance of winning when k is even
  """

  For odd k: 1/2
  For even k: starter wins with p = (k div 2)+1 / (k+1)


k: 1
var : winner
0: 1/2 (0.5)
1: 1/2 (0.5)
mean: 1/2 (0.5)

k: 2
var : winner
0: 2/3 (0.6666666666666666)
1: 1/3 (0.3333333333333333)
mean: 1/3 (0.3333333333333333)

k: 3
var : winner
0: 1/2 (0.5)
1: 1/2 (0.5)
mean: 1/2 (0.5)

k: 4
var : winner
0: 3/5 (0.6)
1: 2/5 (0.4)
mean: 2/5 (0.4)

k: 5
var : winner
0: 1/2 (0.5)
1: 1/2 (0.5)
mean: 1/2 (0.5)

k: 6
var : winner
0: 4/7 (0.5714285714285714)
1: 3/7 (0.42857142857142855)
mean: 3/7 (0.42857142857142855)

k: 7
var : winner
0: 1/2 (0.5)
1: 1/2 (0.5)
mean: 1/2 (0.5)

k: 8
var : winner
0: 5/9 (0.5555555555555556)
1: 4/9 (0.4444444444444444)
mean: 4/9 (0.4444444444444444)

k: 9
var : winner
0: 1/2 (0.5)
1: 1/2 (0.5)
mean: 1/2 (0.5)

k: 10
var : winner
0: 6/11 (0.5454545454545454)
1: 5/11 (0.45454545454545453)
mean: 5/11 (0.45454545454545453)



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model k)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define balls (append (ones-list k "black") (list "red")))

   ; (define s (shuffle balls))
   (define s (draw-without-replacement (length balls) balls))
   

   (define (f a i)
     (if (eq? (first a) "red")
         i
         (f (rest a) (add1 i))))

   (define a (f s 0))
   (define winner (modulo a 2))

   (list ; a
         winner)
   

   )
)

(for ([k (range 1 11)])
      (show "k" k)
(show-marginals (model k)
                (list  ; "a"
                       "winner"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
)

