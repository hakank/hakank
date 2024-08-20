#| 

  Bertrand's Box paradox in Racket Gamble.

  From https://towardsdatascience.com/five-paradoxes-with-probabilities-that-will-puzzle-you-2f71201d6ee8
  """
  3. Bertrand's Box Paradox

  If you are familiar with the Montey Hall problem, this paradox is quite 
  similar. In front of us there are three boxes:

    [image of three boxes]

  One box contains two silver coins, one box contains two gold coins and 
  one box contains a gold- and a silver coin. We do not know which coins 
  are in which box. Now, we pick a random box and blindly draw a coin from 
  our box. It’s a gold coin!

  Now, the question is:

    What’s the probability that the second coin in our box is also a gold coin?

  My naive (and wrong) answer when encountering the problem for the first 
  time was ½. I thought that because we drew a gold coin, our box is either 
  the one with the two gold coins or the one with the mixed coins. In the 
  first case, we would draw another gold coin and in the second case, we 
  wouldn’t. Therefore, I presumed the probability should be ½.

    The real probability is ⅔.

  The reason for that is that the first gold coin we drew could either be 
  the only gold coin in the mixed box, the first gold coin in the solely 
  golden box, or the second gold coin in the solely golden box. And in 
  two of these three possibilities, we will draw another gold coin.
  """

  Here we define the boxes as:
  - box1: 2 gold coins
  - box2: 1 gold and 1 silver coin
  - box3: 2 silver coins

  This is a port of my WebPPL model bertrands_paradox.wppl

  var : coin1
  gold: 1 (1.0)

  var : coin2
  gold: 2/3 (0.6666666666666666)
  silver: 1/3 (0.3333333333333333)

  var : box
  b1: 2/3 (0.6666666666666666)
  b2: 1/3 (0.3333333333333333)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (bertrands-paradox)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   ;; There are 3 boxes
   (define boxes (list "b1" "b2" "b3"))
   
   ;; We pick a box randomly
   (define box (uniform-draw boxes))
   
   (define coins (vector "gold" "silver"))
   
   ;; We pick a coin from the selected box
   (define coin1
     (cond
       [(eq? box "b1") (categorical-vw coins (vector 1   0))]
       [(eq? box "b2") (categorical-vw coins (vector 1/2 1/2))]
       [else           (categorical-vw coins (vector 0   1))]))
   
   ;; We observe that it's a gold coin.
   (observe/fail (eq? coin1 "gold"))
   
   ;; What is the probability that the second coins from the
   ;; same box is also a gold coin?
   (define coin2
     (cond
       [(eq? box "b1") (categorical-vw coins (vector 1 0))]
       [(eq? box "b2") (categorical-vw coins (vector 0 1))]
       [else           (categorical-vw coins (vector 0 1))]))
   
   (list coin1 coin2 box)
   
   )
  )

(show-marginals (bertrands-paradox) '("coin1" "coin2" "box"))
                
