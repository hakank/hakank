#| 

  Martin Gardner's Odds on Kings in Gamble.

  From BL "Martin Gardner’s Odds On Kings - A Probability Puzzle"
  https://medium.com/math-games/martin-gardners-odds-on-kings-58dbf416a598
  """
  Six playing cards are lying face down on the table. You have been told that two 
  and only two of them are kings, but you do not know the position of the kings. 
  You pick two cards at random and turn them face up.

  Which is the most likely:
  (1) There will be at least one king among the two cards?
  (2) There will be no king among the two cards?

  """

  [See below for the variant with 8 cards and 3 kings]

  Generallized version for k*2  + 2 cards and k kings, k = 1..5

(* 4 cards 1 kings)
variable : two-cards
(0 0): 1/2 (0.5)
(0 1): 1/4 (0.25)
(1 0): 1/4 (0.25)

variable : at-least-one-king
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)

variable : no-king
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)

(* 6 cards 2 kings)
variable : two-cards
(0 0): 2/5 (0.4)
(0 1): 4/15 (0.26666666666666666)
(1 0): 4/15 (0.26666666666666666)
(1 1): 1/15 (0.06666666666666667)

variable : at-least-one-king
#t: 3/5 (0.6)
#f: 2/5 (0.4)
mean: 3/5 (0.6)

variable : no-king
#f: 3/5 (0.6)
#t: 2/5 (0.4)
mean: 2/5 (0.4)

(* 8 cards 3 kings)
variable : two-cards
(0 0): 5/14 (0.35714285714285715)
(0 1): 15/56 (0.26785714285714285)
(1 0): 15/56 (0.26785714285714285)
(1 1): 3/28 (0.10714285714285714)

variable : at-least-one-king
#t: 9/14 (0.6428571428571429)
#f: 5/14 (0.35714285714285715)
mean: 9/14 (0.6428571428571429)

variable : no-king
#f: 9/14 (0.6428571428571429)
#t: 5/14 (0.35714285714285715)
mean: 5/14 (0.35714285714285715)

(* 10 cards 4 kings)
variable : two-cards
(0 0): 1/3 (0.3333333333333333)
(0 1): 4/15 (0.26666666666666666)
(1 0): 4/15 (0.26666666666666666)
(1 1): 2/15 (0.13333333333333333)

variable : at-least-one-king
#t: 2/3 (0.6666666666666666)
#f: 1/3 (0.3333333333333333)
mean: 2/3 (0.6666666666666666)

variable : no-king
#f: 2/3 (0.6666666666666666)
#t: 1/3 (0.3333333333333333)
mean: 1/3 (0.3333333333333333)

(* 12 cards 5 kings)
variable : two-cards
(0 0): 7/22 (0.3181818181818182)
(0 1): 35/132 (0.26515151515151514)
(1 0): 35/132 (0.26515151515151514)
(1 1): 5/33 (0.15151515151515152)

variable : at-least-one-king
#t: 15/22 (0.6818181818181818)
#f: 7/22 (0.3181818181818182)
mean: 15/22 (0.6818181818181818)

variable : no-king
#f: 15/22 (0.6818181818181818)
#t: 7/22 (0.3181818181818182)
mean: 7/22 (0.3181818181818182)

  What is the probabilities when k -> infinity?

  This took 1 minute to calculate:
(* 20000002 cards 10000000 kings)
variable : two-cards
(0 0): 1666667/6666667 (0.2500000374999981)
(0 1): 16666670000000/66666676666667 (0.25000001249999687)
(1 0): 16666670000000/66666676666667 (0.25000001249999687)
(1 1): 16666665000000/66666676666667 (0.24999993750000812)

variable : at-least-one-king
#t: 5000000/6666667 (0.7499999625000019)
#f: 1666667/6666667 (0.2500000374999981)
mean: 5000000/6666667 (0.7499999625000019)

variable : no-king
#f: 5000000/6666667 (0.7499999625000019)
#t: 1666667/6666667 (0.2500000374999981)
mean: 1666667/6666667 (0.2500000374999981)

  So when k -> infinity it seems the probably of at least one king 
  is 3/4 and probablity of no king is 1/4

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")


(define (model n num-kings)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
  
   (define cards (append (rep num-kings 1) (rep (- n num-kings) 0)))
   (define two-cards (draw-without-replacement 2 cards))

   (define at-least-one-king (>= (sum two-cards) 1))
   (define no-king (= (sum two-cards) 0))

   (list two-cards
         at-least-one-king
         no-king
         )

   )
)

(for ([k (range 1 6)])
  (show2 "*" (+ (* k 2) 2) "cards" k "kings")
  (show-marginals (model (+ (* k 2) 2) k)
                (list  "two-cards"
                       "at-least-one-king"
                       "no-king"
                     )
                ))



#|

  Using hypergeometric probability distribution (hypergeometric2 from gamble_distributions.rkt)

(* 6 cards 2 kings)
variable : at-least-one-king
3/5: 1 (1.0)
mean: 3/5 (0.6)

variable : no-king
2/5: 1 (1.0)
mean: 2/5 (0.4)

(* 8 cards 3 kings)
variable : at-least-one-king
9/14: 1 (1.0)
mean: 9/14 (0.6428571428571429)

variable : no-king
5/14: 1 (1.0)
mean: 5/14 (0.35714285714285715)

(* 12 cards 5 kings)
variable : at-least-one-king
15/22: 1 (1.0)
mean: 15/22 (0.6818181818181818)

variable : no-king
7/22: 1 (1.0)
mean: 7/22 (0.3181818181818182)

(* 20000000002 cards 10000000000 kings)
variable : at-least-one-king
5000000000/6666666667: 1 (1.0)
mean: 5000000000/6666666667 (0.7499999999625)

variable : no-king
1666666667/6666666667: 1 (1.0)
mean: 1666666667/6666666667 (0.2500000000375)

(* 200000000000000002 cards 100000000000000000 kings)
variable : at-least-one-king
50000000000000000/66666666666666667: 1 (1.0)
mean: 50000000000000000/66666666666666667 (0.75)

variable : no-king
16666666666666667/66666666666666667: 1 (1.0)
mean: 16666666666666667/66666666666666667 (0.25)

|#
(displayln "\nUsing hypergeometric2_cdf\n")
(define (model2 n num-kings)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define num-drawn-cards 2)
   (define num-successes num-kings)
   (define num-total-cards n)
   (define at-least-one-king (- 1 (hypergeometric2_cdf num-drawn-cards num-successes num-total-cards 0)))
   (define no-king (hypergeometric2_cdf num-drawn-cards num-successes num-total-cards 0))

   (list at-least-one-king
         no-king
         )

   )
)

(for ([k (list 2 3 5 10000000000 100000000000000000)])
  (show2 "*" (+ (* k 2) 2) "cards" k "kings")
  (show-marginals (model2 (+ (* k 2) 2) k)
                (list  "at-least-one-king"
                       "no-king"
                     )
                ))



#|
  From the same Medium post:
  """
  A Challenge Ahead: 
  [The Dealer's Twist - Bigger Deck
    
  Eight cards lie face down: exactly three are kings. 
  You pick three cards at random and set them aside, still face
  down.
  The dealer peeks at the other five and turns one card face up.
  It is not a king.
  1. They contain at least one king.
  2. They contain no kings.
  
  Is (1) or (2) more likely to happen.
  """

  The issue here is if the dealer selects a card that is not a king,
  or just turn any card which happens to be no a king.   
  Here are three slightly different versions:
  1) The dealer just pick any card and that happens to be a non king. 
     This is representing by observing that the first of the 
     dealer's five card is not a king
  2) The dealer actively pick a card that is not a king.
     Here we sort the dealer's card (0 for no king and 1 for king), 
     pick the first and see that it's not a king. 
     And that would be impossible since at least the first two cards 
     must be non-kings.

     This interpretation seems to be the most likely given the
     description.

  0) We do not observe anything.
     This corresponds to the fact that the dealer does not show any card.


  Here are the probabilities:

Model3: A Challenge Ahead
(The dealer shows a card as a no king: 1)
variable : my-cards
(0 1 0): 6/35 (0.17142857142857143)
(0 0 1): 6/35 (0.17142857142857143)
(1 0 0): 6/35 (0.17142857142857143)
(1 1 0): 4/35 (0.11428571428571428)
(1 0 1): 4/35 (0.11428571428571428)
(0 1 1): 4/35 (0.11428571428571428)
(0 0 0): 4/35 (0.11428571428571428)
(1 1 1): 1/35 (0.02857142857142857)

variable : at-least-one-king
#t: 31/35 (0.8857142857142857)
#f: 4/35 (0.11428571428571428)
mean: 31/35 (0.8857142857142857)

variable : no-king
#f: 31/35 (0.8857142857142857)
#t: 4/35 (0.11428571428571428)
mean: 4/35 (0.11428571428571428)

(The dealer shows a card as a no king: 2)
variable : my-cards
(0 1 0): 5/28 (0.17857142857142858)
(0 0 1): 5/28 (0.17857142857142858)
(1 0 0): 5/28 (0.17857142857142858)
(0 0 0): 5/28 (0.17857142857142858)
(1 1 0): 5/56 (0.08928571428571429)
(1 0 1): 5/56 (0.08928571428571429)
(0 1 1): 5/56 (0.08928571428571429)
(1 1 1): 1/56 (0.017857142857142856)

variable : at-least-one-king
#t: 23/28 (0.8214285714285714)
#f: 5/28 (0.17857142857142858)
mean: 23/28 (0.8214285714285714)

variable : no-king
#f: 23/28 (0.8214285714285714)
#t: 5/28 (0.17857142857142858)
mean: 5/28 (0.17857142857142858)

(The dealer shows a card as a no king: 0)
variable : my-cards
(0 1 0): 5/28 (0.17857142857142858)
(0 0 1): 5/28 (0.17857142857142858)
(1 0 0): 5/28 (0.17857142857142858)
(0 0 0): 5/28 (0.17857142857142858)
(1 1 0): 5/56 (0.08928571428571429)
(1 0 1): 5/56 (0.08928571428571429)
(0 1 1): 5/56 (0.08928571428571429)
(1 1 1): 1/56 (0.017857142857142856)

variable : at-least-one-king
#t: 23/28 (0.8214285714285714)
#f: 5/28 (0.17857142857142858)
mean: 23/28 (0.8214285714285714)

variable : no-king
#f: 23/28 (0.8214285714285714)
#t: 5/28 (0.17857142857142858)
mean: 5/28 (0.17857142857142858)


  Note that scenario 2 and 0 give the same probabilities,
  which indicates that the step of the dealer showing a 
  non-king does not matter.


|#

(define (model3 observe)
  (enumerate

   (define king 1)
   (define not-king 0)
   
   (define num-kings 3)
   (define num-cards 8)
   (define take-cards 3)
   
   (define cards (append (rep num-kings king) (rep (- num-cards num-kings) not-king)))
   (define shuffled-cards (draw-without-replacement num-cards cards))
   (define my-cards (take shuffled-cards take-cards))
   (define the-rest (drop shuffled-cards take-cards))

   ; Here's the tricky part: 
   ; """
   ; The dealer peeks at the other five and turns one card face up.
   ; It is not a king.
   ; """
   (cond
     [(= observe 1)
      ; One interpretation: The first of the rest five cards is not a king.      
      (observe/fail (not (eq? king (first the-rest))))
     ]
     [(= observe 2)
      ; Ensure that there can not be a king at the first card
      (observe/fail (not (eq? king (first (sort the-rest <)))))]
     
     [(= observe 0)
      ; No observe
       #t]
      )

   
   
   (define s (sum my-cards))
   (define at-least-one-king (>= s 1))
   (define no-king (= s 0))

   (list my-cards
         at-least-one-king
         no-king
         )

   )
)

(displayln "\nModel3: A Challenge Ahead")
(for ([observe '(1 2 0)])
  (show2 "The dealer shows a card as a no king:" observe)
  (show-marginals (model3 observe)
                  (list  "my-cards"
                         "at-least-one-king"
                         "no-king"
                         )
                  ))
