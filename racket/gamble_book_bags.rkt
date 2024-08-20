#| 

  Book bags in Racket Gamble.

  From the Netica model Book Bags.neta
  """
  Book Bags                  Copyright 1998 Norsys Software Corp.

  There are two book bags each containing 10 poker chips.  In one
  bag there are 7 red and 3 blue.  In the other bag there are 3 
  red and 7 blue. Five chips are drawn out of one of the bags and 
  shown to the subject (one at a time then returned to the bag).  
  The subject does not know which bag the chips came from.  
  There is an equal chance that the draws are made from either
  bag. After each draw the subject reports which bag he believes
  the chips are coming from and provides a probability that the 
  chips are being drawn from that bag.
     
  The problem comes from the early "revision of judgment" work 
  that indicated that people were conservative with respect to Bayes.   
  """
 
  This is a port of my WebPPL model book_bags.rkt

seeing 0 blue balls
var : bag
bag1: 0.5
bag2: 0.5

seeing 1 blue balls
var : bag
bag2: 0.7
bag1: 0.30000000000000004

seeing 2 blue balls
var : bag
bag2: 0.8448275862068966
bag1: 0.1551724137931035

seeing 3 blue balls
var : bag
bag2: 0.927027027027027
bag1: 0.072972972972973

seeing 4 blue balls
var : bag
bag2: 0.967365028203062
bag1: 0.03263497179693797

seeing 5 blue balls
var : bag
bag2: 0.9857478005865102
bag1: 0.014252199413489746


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")


(define (book-bags test)
  (enumerate

    (define bag (categorical-vw2 (vector 0.5 0.5) (vector "bag1" "bag2")))
    (define colors (vector "red" "blue"))
    
    ; (We don't need mem)
    (define (draw i) (if (eq? bag "bag1")
                         (categorical-vw2 (vector 7 3) colors)
                         (categorical-vw2 (vector 3 7) colors)))
                
    ;; Observe t number of blue 
    (for ([t (range test)])
      (observe/fail (eq? (draw t) "blue"))
      )

    (list bag)
   )
  )

(for ([test (range 6)])
      (displayln (format "seeing ~a blue balls" test))
      (show-marginals (book-bags test) (list "bag"))
      )
