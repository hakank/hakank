#| 

  Four cards in Racket/Gamble 

  From Bar-Hillel & Falk "Some teasers concerning conditional probabilities" 
  """
  Problem 3:
  A deck of four cards consists of the ace of spades, the ace of clubs, the deuce of spades, 
  and the deuce of clubs. A hand of two cards is randomly dealt from this deck. What 
  is the probability that it contains both aces if we know it contains at least one?

  The answer to this problem is traditionally agreed upon to be l/S, following the reasoning 
  that five equiprobable hands are compatible with the conditioning event (only the double 
  deuce hand is ruled outj, and just one of these contains both aces.

  Now compare Problem 3 to the following: 

  Problem 4:
  Like Problem 3, but the question is: What is the probability that the hand contains 
  both aces if we know it contains the ace of spades’.
  """

  Here are three runs:
  * no observation (obs: #f)
  * model1: At least one ace
  * model2: It contains ace of spades

  * obs: #f
  var : selected
  (2C AS): 1/12 (0.08333333333333333)
  (2S AC): 1/12 (0.08333333333333333)
  (AC AS): 1/12 (0.08333333333333333)
  (2C 2S): 1/12 (0.08333333333333333)
  (2S 2C): 1/12 (0.08333333333333333)
  (AC 2S): 1/12 (0.08333333333333333)
  (AS 2C): 1/12 (0.08333333333333333)
  (2C AC): 1/12 (0.08333333333333333)
  (AS 2S): 1/12 (0.08333333333333333)
  (2S AS): 1/12 (0.08333333333333333)
  (AS AC): 1/12 (0.08333333333333333)
  (AC 2C): 1/12 (0.08333333333333333)

  var : num_aces
  1: 2/3 (0.6666666666666666)
  0: 1/6 (0.16666666666666666)
  2: 1/6 (0.16666666666666666)
  mean: 1 (1.0)

  var : p
  #f: 5/6 (0.8333333333333334)
  #t: 1/6 (0.16666666666666666)
  mean: 1/6 (0.16666666666666666)

  * obs: model1
  var : selected
  (2C AS): 1/10 (0.1)
  (2S AC): 1/10 (0.1)
  (AC AS): 1/10 (0.1)
  (AC 2S): 1/10 (0.1)
  (AS 2C): 1/10 (0.1)
  (2C AC): 1/10 (0.1)
  (2S AS): 1/10 (0.1)
  (AS 2S): 1/10 (0.1)
  (AS AC): 1/10 (0.1)
  (AC 2C): 1/10 (0.1)

  var : num_aces
  1: 4/5 (0.8)
  2: 1/5 (0.2)
  mean: 6/5 (1.2)

  var : p
  #f: 4/5 (0.8)
  #t: 1/5 (0.2)
  mean: 1/5 (0.2)

  * obs: model2
  var : selected
  (2C AS): 1/6 (0.16666666666666666)
  (AS 2C): 1/6 (0.16666666666666666)
  (AS 2S): 1/6 (0.16666666666666666)
  (2S AS): 1/6 (0.16666666666666666)
  (AS AC): 1/6 (0.16666666666666666)
  (AC AS): 1/6 (0.16666666666666666)

  var : num_aces
  1: 2/3 (0.6666666666666666)
  2: 1/3 (0.3333333333333333)
  mean: 4/3 (1.3333333333333333)

  var : p
  #f: 2/3 (0.6666666666666666)
  #t: 1/3 (0.3333333333333333)
  mean: 1/3 (0.3333333333333333)


  Compare with:
  * gamble_two_children_problem.rkt
  * gamble_how_many_sons.rkt

  This is a port of my WebPPL model four_cards.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model [obs #f])
  (show "* obs" obs)
  (enumerate
   
   (define cards '("AS" "AC" "2S" "2C"))
   (define selected (draw-without-replacement 2 cards))

   (define num_aces (sum (map (lambda (v) (count-occurrences-eq v selected)) '("AS" "AC"))))

   (when obs
     (cond
       ; We observe one ace
       [(eq? obs "model1") (observe/fail (>= num_aces 1))]
       ; We observe that first card is the Ace of spades
       [(eq? obs "model2") (observe/fail (= (count-occurrences-eq "AS" selected) 1))]
       )
     )
        
   (define p (= num_aces 2))

   (list selected
         num_aces
         p
        )

   )
)

(for ([obs '(#f "model1" "model2")])
  (show-marginals (model obs)
                  (list  "selected"
                         "num_aces"
                         "p"
                         ))
)
