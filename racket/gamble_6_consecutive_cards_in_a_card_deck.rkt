#| 

  6 consecutive cards in a card deck in Racket Gamble.

  https://mathematica.stackexchange.com/questions/256381/what-is-the-probability-that-there-are-three-or-more-consecutive-numbers-in-thes
  """
  What is the probability that there are three or more consecutive numbers in these six cards?

  52 poker cards (except the red and black jokers), 6 cards are drawn from them. What is 
  the probability that there are three or more consecutive numbers in these six cards?

  The problem have been solved by @BGM in mathematics site. But how to solve this general problem 
  if using mathematica program?
  """

  According to
  https://math.stackexchange.com/questions/4264826/what-is-the-probability-that-there-are-three-or-more-consecutive-numbers-in-thes/4265011#4265011

  the answer is 924163/2544815 ~ 0.363155.

  Let's check that.

  First. note that 
  > (/ 131877.0 363545)
  0.3627528916640306

  But that doesn't help so much since this model give a slightly smaller value

  * enumerate (58s)

  var : c
  0: 131367/195755 (0.6710786442236469)
  1: 124756/508963 (0.24511801447256482)
  2: 35408/508963 (0.06956890775950314)
  3: 32128/2544815 (0.012624886288394244)
  4: 4096/2544815 (0.0016095472558908996)
  mean: 155804/363545 (0.42856867788031744)

  var : p
  #f: 131367/195755 (0.6710786442236469)
  #t: 64388/195755 (0.3289213557763531)
  mean: 64388/195755 (0.3289213557763531)

  I.e. p=0.3289213557763531  which is less than the value of 0.3627528916640306 above.
  So what have I missed?


  This is a port of my WebPPL model 6_consecutive_cards_in_a_card_deck.wppl 
  (The WebPPL model give the same result as this Gamble model.)
 
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; Values in the deck: 1..13 x 4
(define deck (flatten (repeat (lambda () (range 1 14) ) 4)))

(define (model)

  (; enumerate ; slow (as expected)
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 52)
   (define m 6) ;; Draw 6 cards
    
   ;; Draw 6 integers from 0..51 without replacement
   (define drawFrom52 (draw-without-replacement m deck))  
   (define draw6 (sort drawFrom52 <))
   
   ; (show2 "draw6" draw6 "diffs" (differences draw6))
   
   ; (show2 "draw6" draw6)
   
   (define c
     (for/sum ([i (- m 2)])
       (boolean->integer (and
                          (= (- (list-ref draw6 (+ i 1)) (list-ref draw6 i)) 1)
                          (= (- (list-ref draw6 (+ i 2)) (list-ref draw6 (+ i 1) )) 1)
                          )
                         )
       ))
   
   ; A simpler and more generalized version of c, although a littler slower,
   ; at least for enumerate (c and p: 58s vs c2 and p2: 1min07s)
   ; For importance-sample, it's about the same time (5.3s for 10000 samples)
   ; (define c2 (count-occurrences-sublist '(1 1) (differences draw6) ))
   
   
   ;; So, does it have at least one occurence of 3 consecutive values?
   (define p (> c 0))
   ; (define p2 (> c2 0))
   ; (define p3 (= c c2))
   
   ; (show2 "draw6" draw6 "c" c "c2" c2 "p" p "p2" p2)

   ; (observe/fail p)
   
   (list c
         ; c2
         p
         ; p2
         ; p3
         )
  
   )
  )

(show-marginals (model)
                (list "c"
                      ; "c2"
                      "p"
                      ; "p2"
                      ; "p3"
                      )
                #:num-samples 10000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:show-stats? #t
                ; #:show-histogram? #t
                )


