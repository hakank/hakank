#| 

  Card problem in Racket Gamble.

  Second assignment of the CPLINT course:
  https://edu.swi-prolog.org/mod/assign/view.php?id=243
  """
  http://cplint.eu/p/cards.swinb
  Cards

  Suppose you have two decks of poker cards (52 cards, 4 suits, 13 ranks: A, K, Q, J, 10, 9, 8, 7, 6, 5, 4, 3, 2).

  Suppose you draw a card from the first deck and one from the second.

  Write a program to compute the probability that in this way you obtain one pair (two cards of the same rank).

  Can you do it with a single probabilistic clause?

  Add code to compute the probability that you draw at least one ace.
  """

  Note that the two cards are from two different deck of cards!


var : pair
#f: 12/13 (0.9230769230769231)
#t: 1/13 (0.07692307692307693)
mean: 1/13 (0.07692307692307693)

var : at_least_an_ace
#f: 144/169 (0.8520710059171598)
#t: 25/169 (0.14792899408284024)
mean: 25/169 (0.14792899408284024)

var : exactly_one_ace
#f: 145/169 (0.8579881656804734)
#t: 24/169 (0.14201183431952663)
mean: 24/169 (0.14201183431952663)

var : two_aces
#f: 168/169 (0.9940828402366864)
#t: 1/169 (0.005917159763313609)
mean: 1/169 (0.005917159763313609)

var : a_king_or_a_queen_in_spades
#f: 625/676 (0.9245562130177515)
#t: 51/676 (0.07544378698224852)
mean: 51/676 (0.07544378698224852)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (cards)
  (enumerate

   (define suits '("hearts"  "spades"  "clubs"  "diamonds"))
   (define ranks '("ace"  "king"  "queen"  "jack"  "v10"  "v9"  "v8"  "v7"  "v6"  "v5"  "v4" "v3"  "v2"))
    
   (define suit (mem (lambda (c) (uniform-draw suits))))
   (define rank (mem (lambda (c) (uniform-draw ranks))))

   ; (displayln (list "rank 0" (rank 0)) "rank 1" (rank 1))
   
   ;; Probability of a pair
   (define pair (eq? (rank 0) (rank 1)))
   
   ;; At least an ace
   (define at_least_an_ace (or (eq? (rank 0) "ace") (eq? (rank 1) "ace")))

   ;; Exactly one ace
   (define exactly_one_ace (xor (eq? (rank 0) "ace") (eq? (rank 1) "ace")))

   ;; Two aces
   (define two_aces (and (eq? (rank 0) "ace") (eq? (rank 1) "ace")))
    
    ;; A King or a Queen in spades
   (define a_king_or_a_queen_in_spades
     (or
      (and (or (eq? (rank 0) "king") (eq? (rank 0) "queen")) (eq? (suit 0) "spades"))
      (and (or (eq? (rank 1) "king") (eq? (rank 1) "queen")) (eq? (suit 1) "spades")))
     )

   (list pair at_least_an_ace exactly_one_ace two_aces a_king_or_a_queen_in_spades)

   )
  )

(show-marginals (cards)
                (list "pair" "at_least_an_ace" "exactly_one_ace" "two_aces" "a_king_or_a_queen_in_spades")                
                )
                  
