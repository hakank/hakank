#| 

  Subset sum problem in Racket/Rosette.

  From Katta G. Murty: "Optimization Models for Decision Making", page 340
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  
  """
  Example 7.8.1
  
  A bank van had several bags of coins, each containing either
  16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
  street, thieves stole some bags. A total of 100 coins were lost.
  It is required to find how many bags were stolen.
  """

  6 bags were stolen: (2 4 0 0 0 0)

 
  Cf xkcd.rkt for a similar problem

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(require "rosette_utils.rkt")

(require rosette/solver/smt/z3)
(current-solver (z3 #:logic 'QF_FD))


(define (subset-sum a total)
  (define n (length a))
  (define x (for/list ([i n])
              (define-symbolic* x integer?)
              (assert (<= 0 x n))
              x))
  (assert (= total (scalar-product x a)))

  (get-all-solutions x)
  )

(define coins '(16 17 23 24 39 40))
(define total 100)
(define sol (first (subset-sum coins total)))
(displayln (format "~a bags were stolen: ~a" (sum sol) sol))
