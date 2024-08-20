#| 

  Five coins problem in Racket Gamble.

  From https://math.stackexchange.com/questions/3633307/bayes-rule-broken
  """
  Bayes' Rule broken?!?!

  This question has been driving me CRAZY for 4 days now. The question comes from 
  the textbook 'One Thousand Exercises in Probability', specifically Exercise 3 in 
  section 1.4. The solution does not make sense! The question goes as follows:

     'A man possesses five coins, two double-headed, two normal and one double-tailed. 
     The man shuts his eyes, picks a coin at random, and tosses the coin. He opens 
     his eyes, sees a head: what is the probability the lower face is also a head?.

  The book gives an answer that is 2/3
  ....
  """

  Output:
  (head : 2/3 (0.6666666666666666))
  (tail : 1/3 (0.3333333333333333))


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (five-coins)
  (enumerate;; probs.
   
   (define coin (categorical-vw (vector "double-head" "normal-coin" "double-tail") (vector 2/5 2/5 1/5)))
    
   ;;  Toss the coin
   (define toss
     (case coin
       [("double-head") "head"]
       [("double-tail") "tail"]
       [else (categorical-vw (vector "head" "tail") (vector 1/2 1/2))]
       ))
   
    ;;  Flip the coin (i.e. turn it around)
   (define flip-coin
     (cond
       [(eq? coin "double-head") "head"]
       [(eq? coin "double-tail") "tail"]
       [(eq? toss "head") "tail"]
       [(eq? toss "tail") "head"]
       [else "xxx"]
       ))
   
   ;;  We observe that the toss give a head.
   (observe/fail (eq? toss "head"))
   
   ;; What is the probability that the other side (flip side)
   ;; is head?
   flip-coin)
  
  )

(show-model (five-coins))
