#| 

  Minesweeper puzzle in Racket/Rosette.

  From gecode/examples/minesweeper.cc:
  """
  A specification is a square matrix of characters. Alphanumeric
  characters represent the number of mines adjacent to that field. 
  Dots represent fields with an unknown number of mines adjacent to 
  it (or an actual mine).
  """
  
  E.g.
       "..2.3."
       "2....."
       "..24.3"
       "1.34.."
       ".....3"
       ".3.3.."
  """
  
  Also see:
  * http://www.janko.at/Raetsel/Minesweeper/index.htm

  * http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
 
  * Ian Stewart on Minesweeper: 
    http://www.claymath.org/Popular_Lectures/Minesweeper/

  * Richard Kaye's Minesweeper Pages
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm

  * Some Minesweeper Configurations
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf
  

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")
; (require rosette/solver/smt/z3)

(require rosette/solver/smt/z3)

(current-solver (z3 #:logic 'QF_FD))


(define (minesweeper grid)
  (let* ([rows (length grid)]
        [cols (length (first grid))]
        [x (make-var-matrix-integer rows cols 0 1)])
    (for* ([i rows]
           [j cols])
      (let ([g-val (list-ref2d grid i j)]
            [x-val (list-ref2d x i j)])
        ; If the grid contains a hint (> -1)
        (when (>= g-val 0)
          (assert (= x-val 0))
          ; Ensure that the neighbors of this hint sums to the hint value
          (assert (= g-val (sum (for*/list ([a (range -1 2)]
                                            [b (range -1 2)]
                                            #:when (and (>= (+ i a) 0) (< (+ i a) rows)
                                                        (>= (+ j b) 0) (< (+ j b) rows)))
                                  (list-ref2d x (+ i a) (+ j b)))))))
          ))

    ; (show "num-solutions" (get-all-solutions x #:debug? #f)) ; Check for unique solution
    
    ; (clear-vc!)
    (define sol (solve #t))
    ; (show "sol" sol)
    (displayln "solution:")
    ; (for-each println (evaluate x sol))
    (show-matrix (for/list ([row x])
                   (evaluate row sol)) #:align 'right)
    ))

        
          
    
#|    
% Problem from Gecode/examples/minesweeper.cc  problem 0
% 
% Solution:
%  1 0 0 0 0 1
%  0 1 0 1 1 0
%  0 0 0 0 1 0
%  0 0 0 0 1 0
%  0 1 1 1 0 0
%  1 0 0 0 1 1
|#
(define u -1)
(define problem-0 (list (list u u 2 u 3 u) 
                        (list 2 u u u u u) 
                        (list u u 2 4 u 3) 
                        (list 1 u 3 4 u u) 
                        (list u u u u u 3) 
                        (list u 3 u 3 u u)))

(define grid problem-0)

(show-matrix grid #:replace? (list u 'u) )
(minesweeper grid)
