#| 

  Tornado Poker in Racket/Gamble 

  From Julian Simon
  """
  You are standing in the warehouse of a playing-card factory
  that has been hit by a tornado.  Cards are scattered everywhere,
  some not yet wrapped and others ripped out of their packages.
  The factory makes a variety of decks - for poker without a joker,
  poker with a joker, and pinochle; magician's decks; decks made of
  paper and others of plastic; cards of various sizes; and so on.
    Two hours from now a friend will join you for a game of
  near-poker with these cards. Each hand will be chosen as randomly
  as possible from the huge heap of cards, and then burned. What
  odds should you attach to getting the combination two-of-a-kind -
  two cards of different or the same suit but of the same number or
  picture - in a five-card draw?
  """

  * Exactly one pair (i.e. no three-of-a-kind, no two pairs, not full hand etc).
  var : p1
  #f: 15361/28561 (0.5378313084275761)
  #t: 13200/28561 (0.46216869157242396)
  mean: 13200/28561 (0.46216869157242396)


  * At least one pair, i.e. including three-of-a-kind etc
  var : p2
  #t: 16681/28561 (0.5840481775848184)
  #f: 11880/28561 (0.4159518224151815)
  mean: 16681/28561 (0.5840481775848184)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define cards (flatten (for/list ([c (range 1 14)])
                (rep 4 c))))

(define (model)
  (enumerate

   ; Pick 5 cards
   (define pick5 (resample 5 cards))
   ; Count the occurrences of the cards
   (define h (hash-values (collect pick5)))

   ; Exactly one pair
   (define p1 (= (length h) 4))
   
   ; At least one pair
   (define p2 (< (length h) 5))
   
   (list p1
         p2)

   )
)

(show-marginals (model)
                (list  "p1"
                       "p2"
                     ))


