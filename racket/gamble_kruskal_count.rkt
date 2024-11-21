#| 

  Kruskal count in Racket/Gamble 

  https://en.wikipedia.org/wiki/Kruskal_count
  """
  The Kruskal count (also known as Kruskal's principle, Dynkin–Kruskal count, 
  Dynkin's counting trick, Dynkin's card trick, coupling card trick or shift coupling) is a 
  probabilistic concept originally demonstrated by the Russian mathematician 
  Evgenii Borisovich Dynkin in the 1950s or 1960s discussing coupling effects and 
  rediscovered as a card trick by the American mathematician Martin David Kruskal in the 
  early 1970s as a side-product while working on another problem.

  ...

  The trick is performed with cards, but is more a magical-looking effect than a 
  conventional magic trick. The magician has no access to the cards, which are manipulated 
  by members of the audience. Thus sleight of hand is not possible. Rather the effect is 
  based on the mathematical fact that the output of a Markov chain, under certain conditions, 
  is typically independent of the input. A simplified version using the hands of a clock 
  is as follows. 

  A volunteer picks a number from one to twelve and does not reveal it to the magician. 
  The volunteer is instructed to start from 12 on the clock and move clockwise by a number 
  of spaces equal to the number of letters that the chosen number has when spelled out. 
  This is then repeated, moving by the number of letters in the new number. The output 
  after three or more moves does not depend on the initially chosen number and therefore 
  the magician can predict it.
  """

  Here are both the clock version and the card version.

  This is a port of my WebPPL model kruskal_count.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

#|
  Clock version

  variable : a
  (3 8 1 4 8 1): 1/3 (0.3333333333333333)
  (4 8 1 4 8 1): 1/4 (0.25)
  (5 9 1 4 8 1): 1/4 (0.25)
  (6 9 1 4 8 1): 1/6 (0.16666666666666666)

  variable : last_a
  1: 1 (1.0)
  mean: 1 (1.0)

|#

;; Twelve -> 0
(define names '("twelve" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven"))
(define (getLen n) (string-length (list-ref names n)))

(define (model1)
  (enumerate
    
   (define (f a)
     (let ((a_len (length a))
           (last_a (last a)))
       (if (> a_len 5) 
           a
           (let ((len (modulo (+ last_a (getLen last_a)) 12)))
             (f (append a (list len)))))))
   
   (define start (random-integer 12))
   (define a (f (list (getLen start))))
   (define last_a (last a))
   (list a
         last_a
         )
            
   )
)

(displayln "Model 1: clock")
(show-marginals (model1)
                (list  "a"
                       "last_a"
                       ))

#|
  Card version

  https://www.ams.org/publicoutreach/feature-column/fcarc-mulcahy6
  """
  Effect: The victim shuffles a deck thoroughly, then secretly picks a number between 1 
  and 10. The cards are dealt out slowly and steadily, face up, the victim's first key 
  card being the one at the position they choose in advance. The value of this card determines 
  how many to deal out to the next key card, e.g., if the key card is a 4 victim counts off 
  four cards, the last being the new key card. Royal cards count 5. The process is repeated 
  as often as is possible. Eventually they will get a key card (perhaps the last card in the 
  deck) which is not followed by enough cards to get to another one; this last key card is 
  the one they remember. No matter how steadily the victim deals, with no pauses to give 
  any hints as to which cards are key cards, you successfully identify their last key card. 
  For many people, this trick grows more mysterious with repetition.

  Method: The method is simple: you yourself follow the same instructions as the victim 
  deals off the cards, picking at random some card among the first ten, and so on, finally 
  arriving at your own last key card. The amazing thing is that with very high probability 
  you'll both have come to the same final key card. In fact, your apparantly independently 
  determined streams will most often flow together somewhere along the way, sometimes before 
  you are even half way through the deck!
  """

  Here are some runs (using enumerate). 
  The first number in a (and positions) is the start number.

  * This one is great:

  variable : a
  (10 11 12 8 11 2 8 12 1 12): 1/10 (0.1)
  (8 3 8 9 11 2 8 12 1 12): 1/10 (0.1)
  (1 10 9 12 8 11 2 8 12 1 12): 1/10 (0.1)
  (3 4 1 3 8 9 11 2 8 12 1 12): 1/10 (0.1)
  (7 1 3 8 9 11 2 8 12 1 12): 1/10 (0.1)
  (4 7 8 9 11 2 8 12 1 12): 1/10 (0.1)
  ...
  (5 6 8 9 11 2 8 12 1 12): 1/10 (0.1)
  (9 7 6 10 4 5 4 6 2 1 12): 1/10 (0.1)
  (2 13 1 3 8 9 11 2 8 12 1 12): 1/10 (0.1)
  (6 9 12 8 11 2 8 12 1 12): 1/10 (0.1)
  
  variable : positions
  (2 7 8 11 19 28 33 35 43 48 49): 1/10 (0.1)
  (4 11 19 28 33 35 43 48 49): 1/10 (0.1)
  (5 11 19 28 33 35 43 48 49): 1/10 (0.1)
  (9 16 22 27 31 36 40 46 48 49): 1/10 (0.1)
  (6 15 20 28 33 35 43 48 49): 1/10 (0.1)
  (8 11 19 28 33 35 43 48 49): 1/10 (0.1)
  ...
  (10 15 20 28 33 35 43 48 49): 1/10 (0.1)
  (3 7 8 11 19 28 33 35 43 48 49): 1/10 (0.1)
  (7 8 11 19 28 33 35 43 48 49): 1/10 (0.1)
  (1 6 15 20 28 33 35 43 48 49): 1/10 (0.1)

  variable : last_a
  12: 1 (1.0)
  mean: 12 (12.0)

  variable : pos
  49: 1 (1.0)
  mean: 49 (49.0)

  * As is this: 

  variable : a
  (5 8 11 11 8 9 5 10 2): 1/10 (0.1)
  ...
  (8 10 11 11 8 9 5 10 2): 1/10 (0.1)

  variable : positions
  (1 10 16 21 26 31 40 45 50): 1/10 (0.1)
  ...
  (5 13 18 23 31 40 45 50): 1/10 (0.1)

  variable : last_a
  2: 1 (1.0)
  mean: 2 (2.0)

  variable : pos
  50: 1 (1.0)
  mean: 50 (50.0)

  * However, there are cases when it doesn't work.

  variable : a
  (10 8 7 10 6 7 3 10 5): 1/10 (0.1)
  ...
  (7 12 13 7 10 3 2 1 9 5 11): 1/10 (0.1)

  variable : positions
  (1 10 18 25 30 36 43 46 51): 1/10 (0.1)
  ...
  (8 13 18 25 30 36 43 46 51): 1/10 (0.1)

  variable : last_a
  5: 4/5 (0.8)
  11: 1/5 (0.2)
  ...
  mean: 31/5 (6.2)

  variable : pos
  51: 4/5 (0.8)
  49: 1/5 (0.2)
  ...
  mean: 253/5 (50.6)


  variable : a
  (10 9 12 11 3 2 6 13 3 10): 1/10 (0.1)
  ...
  (9 8 9 13 9 13 3 10): 1/10 (0.1)

  variable : positions
  (2 9 17 26 31 40 45 48): 1/10 (0.1)
  ...
  (8 9 17 26 31 40 45 48): 1/10 (0.1)

  variable : last_a
  10: 9/10 (0.9)
  9: 1/10 (0.1)
  mean: 99/10 (9.9)

  variable : pos
  48: 9/10 (0.9)
  46: 1/10 (0.1)
  mean: 239/5 (47.8)

|#

; Count value of each card (the first is just a dummy since we are using values of 1..13)
(define card_len '(0 1 2 3 4 5 6 7 8 9 5 5 5 5))
(define r (range 1 14))
; Note that we define the deck of card outside the model
; So the model just look at that instance
(define cards (draw-without-replacement 52 (flatten (rep 4 r))))
(define (model2)
  (enumerate
   ; importance-sampler

   (define len (length cards))
   
   (define (f pos a positions)
     (if (>= pos len)
         (list a pos positions)
         (let* ([card (list-ref cards pos)]
                [next_pos (+ pos (list-ref card_len card))])
           (if (>= next_pos len)
               (list a pos positions)
               (let ([next_card (list-ref cards next_pos)])
                 (f next_pos
                    (append a (list next_card))
                    (append positions (list next_pos))))))))
   
   (define start (add1 (random-integer 10)))
   (define res (f start (list start (list-ref cards start)) (list start)))
   (define a (first res))
   (define pos (second res))
   (define positions (third res))
   (define last_a (last a))
   
   (list a
         positions
         last_a
         pos
         )
   
   )
  )

(displayln "Model 2: cards")
(show-marginals (model2)
                (list  "a"
                       "positions"
                       "last_a"
                       "pos"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )


