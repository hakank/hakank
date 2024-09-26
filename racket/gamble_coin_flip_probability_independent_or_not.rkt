#| 

  Coin flip probability independent or not? in Racket/Gamble.

  https://math.stackexchange.com/questions/4492477/coin-flip-probability-independent-or-not
  """
  Coin Flip Probability Independent or Not?

  I give you a hat which has 10 coins inside of it. 1 out of the 10 have two heads on it, and 
  the rest of them are fair. You draw a coin at random from the jar and flip it 5 times. 
  If you flip heads 5 times in a row, what is the probability that you get heads on 
  your next flip?

  I tried to approach this question by using Bayes: Let R
  be the event that the coin with both heads is drawn and F be the event that 5 heads are 
  flipped in a row. Then
     P(R|F)=P(F|R)P(R)P(F)=1⋅1/101⋅1/10+1/25⋅9/10=32/41

  Thus the probability that you get heads on the next flip is

  P(H|R)P(R)+P(H|R′)P(R′)=1⋅32/41+1/2⋅(1−32/41)=73/82

  However, according to my friend, this is a trick question because the flip after 
  the first 5 flips is independent of the first 5 flips, and therefore the correct 
  probability is
   1⋅1/10+1/2⋅9/10=11/20

  Is this true or not?
  """

  We have n number of coins, of which 1 is a two head, and the rest (n-1) are fair.
  We toss the coin m times and get head all the time. What is the probability 
  that we get a head on the next toss?

  This model confirms the answer of p=73/82 = 0.89024390243902439024

  var : p
  #t: 73/82 (0.8902439024390244)
  #f: 9/82 (0.10975609756097561)
  mean: 73/82 (0.8902439024390244)

  Some other experiments:
  n: 10 m: 1
  var : p
  #t: 13/22 (0.5909090909090909)
  #f: 9/22 (0.4090909090909091)
  mean: 13/22 (0.5909090909090909)

  n: 10 m: 10
  var : p
  #t: 2057/2066 (0.9956437560503388)
  #f: 9/2066 (0.004356243949661181)
  mean: 2057/2066 (0.9956437560503388)

  n: 5 m: 3
  var : p
  #t: 5/6 (0.8333333333333334)
  #f: 1/6 (0.16666666666666666)
  mean: 5/6 (0.8333333333333334)

  n: 3 m: 2
  var : p
  #t: 5/6 (0.8333333333333334)
  #f: 1/6 (0.16666666666666666)
  mean: 5/6 (0.8333333333333334)

  n: 30 m: 3
  var : p
  #t: 45/74 (0.6081081081081081)
  #f: 29/74 (0.3918918918918919)
  mean: 45/74 (0.6081081081081081)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model n m)
  (enumerate

   ; Select a coin: 1 coins has two heads, the rest (n-1) are fair
   (define coin (categorical-vw2 (vector (/ 1 n) (/ (- n 1) n)) (vector "two_head" "fair")))
        
   ; What is the result of the tosses
   (define (toss t) 
     (if (eq? coin "two_head")
         "head"
         (categorical-vw2 (vector 1/2 1/2) (vector "head" "tail"))))
        
   ; We toss head m (5) times
   (for ([i m]) 
     (observe/fail (eq? (toss i) "head")))
        
   ; What is the probability of head the next toss?
   (define p (eq? (toss m) "head"))
        
   (list p)
   
   )
)

(for ([nm '((10 5) (10 1) (10 10) (5 3) (3 2) (30 3))])
  (let ([n (first nm)]
        [m (second nm)])
    (displayln (format "n: ~a m: ~a" n m))
    (show-marginals (model n m)
                    (list  "p"
                           ))
    ))

