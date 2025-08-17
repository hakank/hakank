#| 

  Wason Selection test in Racket/Gamble 

  This Racket/Gamble model was inspired by
  Bayesian Adventures (Pascal Bercker): "Modeling the Wason Selection Task ..."
  https://www.youtube.com/watch?v=xdsSiXmkoZ0
  """
  Each card has a number on one side and a letter on the other side.
  Which card(s) must you turn over in order to test the truth of
  the proposition
     IF a card shows a vowel on one face, THEN it's an even number
     on the other
  ?

  The cards are
     A   K   4   7

  [We should turn A and 7.]
  """

  The video contains a wonderful explanation of this problem using 
  Bayesian Networks (Netica). 


  The following rule is probably simpler to understand (and is also 
  discussed in the video):
    IF P is drinking alchohol THEN P must be 18 year or older,
  Here are 4 cards representing people that we know are drinking something:
     Drinking Beer   Drinking Cola   Age is 20    Age is 17

  Which card should be checked to check if the rule is enforced:
  Answer: We should turn the "cards":
  - "Drinking beer" (to check the age)
  - "Age is 17"  (to check the drink)

  The two other case are irrelevant:
  - Drinking Cola: no age limit
  - Age is 20: They can drink whatever they like.
 
  This is exactly the same situation as the Wason test above.
 

  Below is the result of the two cases using the model. 

  The variable rule-ok represents if the rule is enforced or not. 
  If it's 100% #t, then we know everything that's needed to know, 
  and we don't have to turn the card.
  However, for the cases when rule-ok is uncertain, we have to turn 
  the card, i.e. the first and fourth cards in both scenarios.
  
  Note: #t and #f represents the facts that we know something
  about the side of the card (alcohol drink, age) and (vowel, even number),
  respectively.
  "N/A" means that we don's know the that side of the card.


  The drinking rule:
  (Case  1 drinking-alcohol: #t age-18-or-older: N/A)
  variable : rule-ok
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  (Case  2 drinking-alcohol: #f age-18-or-older: N/A)
  variable : rule-ok
  #t: 1 (1.0)
  mean: 1 (1.0)

  (Case  3 drinking-alcohol: N/A age-18-or-older: #t)
  variable : rule-ok
  #t: 1 (1.0)
  mean: 1 (1.0)

  (Case  4 drinking-alcohol: N/A age-18-or-older: #f)
  variable : rule-ok
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)


  The Wason selection test:
  (Case  A is-wowel: #t is-even: N/A)
  variable : rule-ok
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)

  (Case  K is-wowel: #f is-even: N/A)
  variable : rule-ok
  #t: 1 (1.0)
  mean: 1 (1.0)

  (Case  4 is-wowel: N/A is-even: #t)
  variable : rule-ok
  #t: 1 (1.0)
  mean: 1 (1.0)

  (Case  7 is-wowel: N/A is-even: #f)
  variable : rule-ok
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)


  Also, see 
  * https://en.wikipedia.org/wiki/Wason_selection_task
  * Pascal Bercker's Medium posts on Bayesian Networks and puzzle solving


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(display "\nThe drinking rule:\n")
(define (model card)
  (enumerate

   (define if-part (flip 1/2))
   (define then-part (flip 1/2))

   (define rule-ok (if if-part then-part #t))

   ; (define id (first card))
   (define s1 (second card)) ; side 1 of the card
   (define s2 (third card))  ; side 2 or the card

   ; We only use observe/fail if we know the value of the side of the card,
   ; i.e. #t or #f.
   (when (boolean? s1) (observe/fail if-part s1))
   (when (boolean? s2) (observe/fail then-part s2))
   
   (list rule-ok)

   )
)

;
; Alcohol rule.
; "N/A": We don't know the value of this side of the card
;        (drinking-alcohol   age-18-or-older)
(define card1 '("Beer"   #t   "N/A")) ; Beer
(define card2 '("Cola"   #f   "N/A")) ; Cola
(define card3 '("20 yo" "N/A"    #t)) ; 20 yo
(define card4 '("17 yo" "N/A"    #f)) ; 17 yo

(define cards (list card1 card2 card3 card4))

(for ([card cards])
  (show2  "Case " (first card) "drinking-alcohol:" (second card) "age-18-or-older:" (third card) )
  (show-marginals (model card)
                  (list "rule-ok"))
  )

;
; The Wason selection test.
;
(display "\nThe Wason selection test:\n")
;            (is-wowel is-even)
(define card1W '("A" #t    "N/A")) ; A
(define card2W '("K" #f    "N/A")) ; K
(define card3W '("4" "N/A"    #t)) ; 4
(define card4W '("7" "N/A"    #f)) ; 7
(define cardsW (list card1W card2W card3W card4W))

(for ([card cardsW])
  (show2 "Case " (first card) "is-wowel:" (second card) "is-even:" (third card) )
  (show-marginals (model card)
                  (list "rule-ok"))
  )


