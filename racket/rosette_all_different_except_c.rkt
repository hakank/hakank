#| 

  Global constraint all-different-except-c in Racket/Rosette.

  Ensures that all variables != c (default 0) are distinct.

  For n=5 (domain 0..5):

  '((0 0 0 0 0) (0 0 0 0 2) (0 0 1 2 3) (0 0 0 0 4) (0 0 0 2 3) (0 0 0 2 5) (0 0 0 2 4) 
    (0 0 0 3 5) (0 0 0 4 5) (0 0 0 0 3) (0 0 0 0 1) (0 0 0 0 5) (0 0 0 1 4) (0 0 0 3 4) 
    (0 0 0 1 2) (0 0 0 1 5) (0 0 0 1 3) (0 0 2 4 5) (0 0 1 3 4) (0 0 1 3 5) (0 0 2 3 4) 
    (0 0 1 4 5) (0 0 1 2 5) (0 1 2 3 5) (1 2 3 4 5) (0 0 1 2 4) (0 2 3 4 5) (0 1 3 4 5) 
    (0 0 2 3 5) (0 0 3 4 5) (0 1 2 3 4) (0 1 2 4 5))
  (num-sols 32)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
(require "rosette_utils.rkt")
(require rosette/solver/smt/z3)
(current-solver (z3 #:logic 'QF_FD))

(define n 5)

(define x (for/list ([i n])
             (define-symbolic* x integer?)
             (assert (<= 0 x n))
             x))


(assert (all-different-except-c x))
(assert (increasing x))

(define sols (get-all-solutions x))
sols
(show "num-sols" (length sols))

