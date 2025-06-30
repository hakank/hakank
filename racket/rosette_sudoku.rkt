#| 

  Sudoku in Racket/Rosette.

  https://en.wikipedia.org/wiki/Sudoku

  This model was originally inspired by the rosette-template example
  https://github.com/racket-templates/rosette-template/tree/master

  but I've implemented more common constraint programming constructs 
  (at least for me and my background in CP).

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")
(require rosette/solver/smt/z3)

(current-solver (z3 #:logic 'QF_FD))

#|
Solution
'(5 3 4 6 7 8 9 1 2)
'(6 7 2 1 9 5 3 4 8)
'(1 9 8 3 4 2 5 6 7)
'(8 5 9 7 6 1 4 2 3)
'(4 2 6 8 5 3 7 9 1)
'(7 1 3 9 2 4 8 5 6)
'(9 6 1 5 3 7 2 8 4)
'(2 8 7 4 1 9 6 3 5)
'(3 4 5 2 8 6 1 7 9)

|#
;; An example game from https://en.wikipedia.org/wiki/Sudoku
;; 0 means blank and is to be filled
(define game '([5 3 0 0 7 0 0 0 0]
               [6 0 0 1 9 5 0 0 0]
               [0 9 8 0 0 0 0 6 0]
               [8 0 0 0 6 0 0 0 3]
               [4 0 0 8 0 3 0 0 1]
               [7 0 0 0 2 0 0 0 6]
               [0 6 0 0 0 0 2 8 0]
               [0 0 0 4 1 9 0 0 5]
               [0 0 0 0 8 0 0 7 9]))


#|
  
  This problem is problem 34 from
  Gecode's sudoku.cpp
  http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
 
  Size : 16 x 16
 
  Solution (1.2s)
 13  9  2 11 15 12 10  1 16  6 14  7  4  3  8  5
  4 12 15 10  3  5 16  8  9 13  1  2  7  6 14 11
  3 14  7  1  4  6  2 13 15  5  8 11 12  9 16 10
 16  5  6  8  9  7 14 11 10  3 12  4 15 13  2  1
 12  7 16  5 10  8 11 15  3  2  6  1 14  4  9 13
  2 13  8  4 12  3  1 14  5 11  7  9 10 15  6 16
  1 11 10 14  2  9  6  4 13 15 16  8  3  7  5 12
  6 15  9  3  5 13  7 16  4 12 10 14 11  2  1  8
 15  3 14 16  1  2  9  5  7  8 11 10  6 12 13  4
  9 10 11 12 16 15  8  7  6  4  5 13  2  1  3 14
  5  8  4  2 13 10  3  6 14  1  9 12 16 11  7 15
  7  6  1 13 11 14  4 12  2 16  3 15  5  8 10  9
 11  4 12  9  7 16  5  2  8 10 13  3  1 14 15  6
  8  2  5  6 14  1 15  9 12  7  4 16 13 10 11  3
 14  1  3 15  6  4 13 10 11  9  2  5  8 16 12  7
 10 16 13  7  8 11 12  3  1 14 15  6  9  5  4  2

|#
(define game34 '(
(13  9  2  0  0  0  0  0 16  0  0  0  4  3  0  0) 
( 4 12 15  0  0  0  0  0  9 13  0  2  0  6 14 11) 
( 0 14  0  1  0  0  0  0 15  0  8 11 12  0  0 10) 
(16  5  6  0  0  0  0  0 10  3 12  0  0  0  0  1) 
( 0  7 16  5 10  8  0  0  0  0  6  1  0  0  0  0) 
( 2  0  0  0 12  0  0  0  0 11  7  0  0  0  0  0) 
( 0  0 10 14  0  9  6  4  0  0 16  0  0  0  0  0) 
( 0 15  9  0  5  0  7  0  4  0  0  0  0  0  0  0) 
( 0  0  0  0  0  2  9  0  0  0  0 10  0 12  0  0) 
( 0  0  0  0  0  0  0  0  6  4  5 13  0  1  0  0) 
( 0  0  0  0 13  0  0  0  0  1  0 12  0 11  7 15) 
( 0  0  0  0  0 14  0 12  2 16  0  0  0  8 10  9) 
(11  0  0  9  0 16  5  2  0  0  0  0  0 14 15  6) 
( 0  2  5  6  0  0 15  0  0  0  0  0 13  0 11  0) 
(14  1  3  0  6  0 13  0  0  0  0  0  0  0  0  7) 
(10  0  0  0  8 11 12  3  0  0  0  0  9  5  4  0)))


; 
; This problem is problem 89 from
; Gecode's sudoku.cpp
; http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
;
; Size : 25 x 25
;
; (This is one of the easiest 25 x 25 instances)
;
;  Solution: 3h9min24s
;  Solution with QD_FD: 11min51.03s
;
;  11 23 13 10 19 16  6  2 24  7  5  9  1 20 17 15  8 18 25  3  4 12 21 22 14
;  15 16  4 22 18 11  8 21 20 10 25  2 14 13 24  7 12 19 23  9 17  5  6  1  3
;  21  1  5 20 25  3 18 15  9 22 11 16  8  4 12 17 14 13  6 24  7 23 19 10  2
;   3  8 12  9 24 19 17 14 23  4  7 21  6 22 10 16 11  1  2  5 15 18 20 13 25
;  17 14  7  6  2  1  5 13 12 25  3 18 19 23 15  4 20 22 10 21 11 16  9 24  8
;  22 19 23 21 13  6  2  3 17 24  4  7 12  1  9 11 15 25 16  8 18 14  5 20 10
;  25 18  2 24  8 22  4 19 16 21 14 11  5 10 13 23 17  6 20  1  9  3 12 15  7
;   6 10 17  3 16  5 12  7  8  9 15 20  2 25 18 22 19 14 24 13  1 11 23 21  4
;   1 11 14 12  9 20 15 10 25 13  6  8 23 16 21 18  4  5  3  7 19 17 22  2 24
;   5 20 15  4  7 14  1 11 18 23 17 19  3 24 22  9  2 21 12 10  6  8 13 25 16
;  20 24 10 13 15 23 11 17 19  3 21  1 16  7  2 12  5  9  4 25  8 22 14 18  6
;   4  5 16 14 12 25 10 18  6  2 23 13 15  8 19  1 24  3 17 22 20 21  7  9 11
;  18 22 21 11  3  8 16 24  4 12  9 17 25 14  5 20 10 15  7  6  2 13  1 19 23
;  19  7  6  2  1  9 13  5 22 15 20 24  4 18 11  8 21 16 14 23 25 10  3 12 17
;   9 17 25  8 23  7 14 20 21  1 12 10 22  6  3  2 13 11 19 18 24 15 16  4  5
;  10 13 19 16 11 18 24  6  3 17  1  5 20 12  7 25  9  2 21 15 23  4  8 14 22
;  12 25  8 15 21 10 19 23 14 11  2  4 13 17 16  3  1  7 22 20  5  9 24  6 18
;  14  4 18  5 22 15 20  9  2 16 19 23 21  3  8 10  6 24 13 17 12  7 25 11  1
;   7  2 24  1 20 12 21 25 13  8 18 22 11  9  6 14 23  4  5 16 10 19 17  3 15
;  23  3  9 17  6  4  7 22  1  5 10 14 24 15 25 19 18 12  8 11 21 20  2 16 13
;  13 12 20 19 10 17  3 16 11  6 22 15  7  5  1 21 25 23 18  2 14 24  4  8  9
;  24  9 11 18 14 13 22  8 10 19 16 25 17 21 23  6  7 20  1  4  3  2 15  5 12
;  16  6 22 25  5  2 23  4 15 18  8 12  9 19 20 24  3 17 11 14 13  1 10  7 21
;   2 21  3 23  4 24  9  1  7 20 13  6 10 11 14  5 16  8 15 12 22 25 18 17 19
;   8 15  1  7 17 21 25 12  5 14 24  3 18  2  4 13 22 10  9 19 16  6 11 23 20
;
(define game89 '(
(11 23 13 10 19 16  6  2 24  7  5  9  1 20 17 15  8 18 25  3  4 12 21 22 14) 
(15 16  0 22  0 11  8  0  0  0 25  0 14  0  0  0 12 19  0  0 17  0  0  0  0) 
( 0  0  0  0  0  0  0  0  0  0  0 16  0  4  0 17  0 13  0 24  0 23 19 10  2) 
( 0  0  0  0  0 19  0 14 23  4  0 21  6 22 10  0 11  0  2  0  0  0  0  0  0) 
(17 14  0  0  2  0  0 13 12  0  0  0  0  0 15  4 20 22 10  0 11  0  9 24  8) 
(22  0  0  0  0  6  2  0  0  0  4  7 12  1  9  0  0  0  0  0  0 14  5  0  0) 
( 0 18  2  0  8 22  0 19 16 21  0  0  0 10 13 23  0  0 20  0  0  3  0 15  7) 
( 0  0 17  3  0  5  0  0  8  9  0  0  0  0 18  0 19  0  0  0  0  0 23 21  0) 
( 1 11  0  0  9  0 15 10 25  0  6  0 23  0  0  0  0  5  3  7  0 17  0  0 24) 
( 0  0  0  0  0  0  1  0  0 23  0  0  0 24  0  0  0 21 12  0  6  8  0 25 16) 
(20 24 10  0 15 23 11 17  0  0  0  0  0  7  0 12  0  0  0  0  0 22  0  0  6) 
( 4  5  0 14 12 25  0 18  0  0 23  0 15  0 19  1  0  0  0 22 20  0  7  9  0) 
(18  0 21  0  0  8  0 24  0  0  9  0 25  0  0  0 10  0  0  0  2  0  1 19  0) 
( 0  0  6  2  1  0 13  0 22  0  0  0  0  0 11  8 21 16  0  0 25  0  0 12 17) 
( 0 17 25  0 23  7 14  0 21  1  0  0  0  0  3  0  0 11  0  0 24  0 16  4  5) 
( 0  0  0  0 11 18 24  0  0  0  0  5  0 12  0 25  0  0  0 15 23  4  8 14  0) 
( 0  0  0 15 21  0  0  0  0  0  2  0 13 17  0  0  1  7  0  0  5  9 24  0  0) 
( 0  0 18  0 22 15  0  0  2 16  0 23  0  0  0 10  6 24  0 17 12  0 25 11  0) 
( 7  2  0  1  0  0 21  0  0  0 18 22  0  9  6 14  0  4  5 16  0  0  0  0  0) 
( 0  0  9  0  0  0  7 22  0  0 10  0 24  0  0  0 18  0  0  0 21  0  0  0  0) 
( 0 12  0 19 10  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0 14  0  4  8  0) 
(24  0 11 18  0  0  0  0  0  0  0 25 17 21  0  6  0  0  1  0  0  0  0  5 12) 
(16  6 22  0  0  0 23  4 15 18  8  0  0  0 20  0  0 17  0 14  0  0  0  0  0) 
( 0 21  0  0  4  0  9  1  7  0  0  0  0 11 14  0 16  8 15  0 22  0 18  0  0) 
( 8 15  0  0  0  0  0  0  5  0 24  3  0  0  4  0  0  0  9  0  0  0  0  0 20)))

#|
  Testing many solutions

     4   x$0   x$1   x$2
   x$3     1   x$4   x$5
   x$6   x$7   x$8     1
   x$9  x$10  x$11  x$12
(len 4 group-size 2)
Solution(s):
  4  2  1  3
  3  1  4  2
  2  4  3  1
  1  3  2  4

  4  3  1  2
  2  1  4  3
  3  4  2  1
  1  2  3  4

  4  3  1  2
  2  1  3  4
  3  4  2  1
  1  2  4  3

  4  2  1  3
  3  1  2  4
  2  3  4  1
  1  4  3  2

  4  2  1  3
  3  1  2  4
  2  4  3  1
  1  3  4  2

  4  3  1  2
  2  1  3  4
  3  2  4  1
  1  4  2  3

|#
(define game4 '([4 0 0 0]
                [0 1 0 0]
                [0 0 0 1]
                [0 0 0 0]))
                

; (define x (make-grid-vars game4)) ; 4 x 4
; (define x (make-grid-vars game)) ; 9 x 9
(define x (make-grid-vars game34)) ; 16 x 16 
; (define x (make-grid-vars game89)) ; 25 x 25

(show-matrix x #:width 6 #:align 'right)

(define xt (transpose x))

(define len (length x))
(define group-size (sqrt len))
(show "len" len "group-size" group-size)
; (show "x" x)


; Distinct rows and columns
(for ([i len])
  ; (assert (apply distinct? (list-ref x i)))
  ; (assert (apply distinct? (list-ref xt i))))
  (assert (all-different (list-ref x i)))
  (assert (all-different (list-ref xt i))))

; Distinct groups
(for* ([i (range 0 len group-size)]
       [j (range 0 len group-size)])
  (let ([group (for*/list ([k group-size]
                           [l group-size])
                 (list-ref2d x (+ i k) (+ j l)))])
    (assert (all-different group))))

(displayln "Solution(s):")
(for ([m (get-all-solutions x #:debug? #f)])
  (show-matrix m)
  (newline)
  )

;; (exit)
;; (define sol (solve #t))
;; sol
;; ; (for-each println (evaluate x sol))
;; (define sol-mat (for/list ([row x])
;;                   (evaluate row sol)))
;; sol-mat
;; (show-matrix sol-mat  #:align 'right)
