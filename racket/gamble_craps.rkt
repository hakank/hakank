#| 

  Craps in Racket/Gamble 

  https://en.wikipedia.org/wiki/Craps
  """
  Craps is a dice game in which players bet on the outcomes of a pair of dice. 
  Players can wager money against each other (playing "street craps") or against 
  a bank ("casino craps").

  ... 
  
  If the come-out roll is 7 or 11, the bet wins.
  If the come-out roll is 2, 3 or 12, the bet loses (known as "crapping out").
  """

  var : d1
  1: 1/6 (0.16666666666666666)
  2: 1/6 (0.16666666666666666)
  3: 1/6 (0.16666666666666666)
  4: 1/6 (0.16666666666666666)
  5: 1/6 (0.16666666666666666)
  6: 1/6 (0.16666666666666666)
  mean: 7/2 (3.5)

  var : d2
  1: 1/6 (0.16666666666666666)
  2: 1/6 (0.16666666666666666)
  3: 1/6 (0.16666666666666666)
  4: 1/6 (0.16666666666666666)
  5: 1/6 (0.16666666666666666)
  6: 1/6 (0.16666666666666666)
  mean: 7/2 (3.5)

  var : s
  7: 1/6 (0.16666666666666666)
  6: 5/36 (0.1388888888888889)
  8: 5/36 (0.1388888888888889)
  5: 1/9 (0.1111111111111111)
  9: 1/9 (0.1111111111111111)
  4: 1/12 (0.08333333333333333)
  10: 1/12 (0.08333333333333333)
  3: 1/18 (0.05555555555555555)
  11: 1/18 (0.05555555555555555)
  2: 1/36 (0.027777777777777776)
  12: 1/36 (0.027777777777777776)
  mean: 7 (7.0)

  var : bet_wins
  #f: 7/9 (0.7777777777777778)
  #t: 2/9 (0.2222222222222222)
  mean: 2/9 (0.2222222222222222)

  var : bet_loses
  #f: 8/9 (0.8888888888888888)
  #t: 1/9 (0.1111111111111111)
  mean: 1/9 (0.1111111111111111)

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

   (define d1 (add1 (random-integer 6)))
   (define d2 (add1 (random-integer 6)))   

   (define s (+ d1 d2))

   (define bet_wins (or (= s 7) (= s 11)))
   (define bet_loses (or (= s 2) (= s 3) (= s 12)))

   (list d1
         d2
         s
         bet_wins
         bet_loses
   )
   )
)

(show-marginals (model)
                (list  "d1"
                       "d2"
                       "s"
                       "bet_wins"
                       "bet_loses"
                       )
                )


