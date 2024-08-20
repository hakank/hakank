#| 

  Rolling dice problem in Racket Gamble.

  From https://dtai.cs.kuleuven.be/problog/tutorial/basic/03_dice.html
  """
  We now consider yet another way to model dice, using fair ones only. This representation 
  allows for convenient use of the results in arithmetic expressions, e.g., to add up the 
  results from several dice. We query for the probabilities of the possible sums we can get 
  from two dice given that the first number is even and the second odd.
  """ 

  (7 : 1/3 (0.3333333333333333))
  (5 : 2/9 (0.2222222222222222))
  (9 : 2/9 (0.2222222222222222))
  (3 : 1/9 (0.1111111111111111))
  (11 : 1/9 (0.1111111111111111))
  (mean: 7.0)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (rolling-dice-3)
  
  (enumerate

   (define roll
     (mem (lambda (d) (uniform-draw '(1 2 3 4 5 6)))))
    
   (define sum
     (mem (lambda (d1 d2) 
        (+ (roll d1) (roll d2)))))

    (define (odd d)
      (= (modulo (roll d) 2) 1))
    
    (define (even d)
      (not (odd d)))
    
    (observe/fail (even "d1"))
    (observe/fail (odd "d2"))

    (sum "d1" "d2")
   )
  )

(show-model (rolling-dice-3) )

