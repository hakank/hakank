#| 

  Binomial dice in Racket Gamble.

  https://reference.wolfram.com/language/ref/BinomialDistribution.html
  """
  Two players roll dice. If the total of both numbers is less than 10, the second player 
  is paid 4 cents; otherwise the first player is paid 9 cents. Is the game fair?:

  ... 

  The game is not fair: mean scores are 1.5 and 3.33333.
  """
   
  Here are three models:
  * Model 1: convoluted and can not be handled by enumerate.
    This is a port of my WebPPL model binomial_dice2.wppl

  * Model 2: Exact model, using set! 
    This was inspired by my PSI model binomial_dice2.psi

  * Model 3: Exact model, set!-free
    This is based on model 2, but is set!-free.


  Model 2 and 3 give the same result:

var : player1
1: 1/6 (0.16666666666666666)
2: 1/6 (0.16666666666666666)
3: 1/6 (0.16666666666666666)
4: 1/6 (0.16666666666666666)
5: 1/6 (0.16666666666666666)
6: 1/6 (0.16666666666666666)
mean: 7/2 (3.5)

var : player2
1: 1/6 (0.16666666666666666)
2: 1/6 (0.16666666666666666)
3: 1/6 (0.16666666666666666)
4: 1/6 (0.16666666666666666)
5: 1/6 (0.16666666666666666)
6: 1/6 (0.16666666666666666)
mean: 7/2 (3.5)

var : total
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

var : player1-sum
0: 5/6 (0.8333333333333334)
9: 1/6 (0.16666666666666666)
mean: 3/2 (1.5)

var : player2-sum
4: 5/6 (0.8333333333333334)
0: 1/6 (0.16666666666666666)
mean: 10/3 (3.3333333333333335)

var : fair?
#f: 1 (1.0)
mean: 0 (0.0)


  I.e. the game is not fail, the mean scores ar 1.5 (player1) and 3.33 (player2).

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

;;
;; This is a port of my WebPPL model binomial_dice2.wppl
;; Too slow and too messy compared to binomial-dice-psi below
;;
(define (binomial-dice2)
  (; enumerate ; Too slow
   ; rejection-sampler
   ; importance-sampler
   mh-sampler

   ; (define N 36)
   (define N 10)   
    
   (define (player1 i) (add1 (random-integer 6)))
   (define (player2 i) (add1 (random-integer 6)))
    
   ;; Find the optimal limit
   (define L (+ 2 (random-integer 11)))

   (define (sum-players i) (+ (player1 i) (player2 i)))
   
   ;; Outcome (seen as player1)
   (define (outcome i) (if (< (sum-players i) L) -4  9))

 
   (define prob1B (>= (sum-players 0) L))
   (define prob2B (<  (sum-players 0) L))

   (define prob1 (/ (for/sum ([i (range N)])
                                  (if (>= (sum-players i) L) 1 0))
                    N))

   (define prob2 (/ (for/sum ([i (range N)])
                                  (if (< (sum-players i) L) 1 0))
                    N))
         
   (define total (for/sum ([i (range N)]) (outcome i)))
    
   (define prob1Times9 (* prob1 9))
   (define prob2Times4 (* prob2 4))
  
    ;; condition(L == 10);
   
   ;; What L would be fair game?
   ;; L: 9
   (define is-fair? (= prob1Times9 prob2Times4))
   (observe/fail (= prob1Times9 prob2Times4))
   
   (list total prob1B prob2B prob1 prob2 prob1Times9 prob2Times4 L is-fair?)
   
   )
   
  )

;; (show-marginals (binomial-dice2)
;;                 (list "total" "prob1B" "prob2B" "prob1" "prob2"
;;                       "prob1Times9" "prob2Times4" "L" "is-fair?")
;;                 #:num-samples 10000
;;                 ; #:truncate-output 5
;;                 )


;;
;; A neater (and exact) model, a port of my PSI model binomial_dice.psi
;; It's nice that one can update the variables.
;;
(define (binomial-dice2-psi)
  (enumerate

   (define limit 10)
   
   (define player1 (add1 (random-integer 6)))
   (define player2 (add1 (random-integer 6)))
    
   (define total (+ player1 player2))

   (define player1-sum 0)
   (define player2-sum 0)   

   ; Update the values
   (if (< total limit)
       (set! player2-sum 4)
       (set! player1-sum 9))

   (define fair? (= player1-sum player2-sum))
   
   (list player1 player2 total player1-sum player2-sum fair?)
     
   )
  
  )

(displayln "\nModel 2 (with set!)")
(show-marginals (binomial-dice2-psi)
                (list "player1" "player2" "total" "player1-sum" "player2-sum" "fair?"))

#|
  
  But is set! really needed for this?
  Nope, here's a set!-free variant, as neat as model 2.
  And the result is exactly as for model 2.
  
|# 
(define (binomial-dice3)
  (enumerate

   (define limit 10)
   
   (define player1 (add1 (random-integer 6)))
   (define player2 (add1 (random-integer 6)))
    
   (define total (+ player1 player2))

   (define outcome (if (< total limit)
                       (list 0 4)  ; player 2
                       (list 9 0)) ; player 1
     )

   (define player1-sum (first outcome))
   (define player2-sum (second outcome)) 

   (define fair? (= player1-sum player2-sum))
   
   (list player1 player2 total player1-sum player2-sum fair?)
     
   )
  
  )

;; (displayln "\nModel 3")
;; (show-marginals (binomial-dice3)
;;                 (list "player1" "player2" "total" "player1-sum" "player2-sum" "fair?"))

