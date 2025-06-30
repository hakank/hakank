#| 

  Broken weights problem in Racket/Rosette.


  From http://www.mathlesstraveled.com/?p=701
  """
  Here's a fantastic problem I recently heard. Apparently it was first
  posed by Claude Gaspard Bachet de Meziriac in a book of arithmetic problems
  published in 1612, and can also be found in Heinrich Dorrie's 100
  Great Problems of Elementary Mathematics.

      A merchant had a forty pound measuring weight that broke
      into four pieces as the result of a fall. When the pieces were
      subsequently weighed, it was found that the weight of each piece
      was a whole number of pounds and that the four pieces could be
      used to weigh every integral weight between 1 and 40 pounds. What
      were the weights of the pieces?

  Note that since this was a 17th-century merchant, he of course used a
  balance scale to weigh things. So, for example, he could use a 1-pound
  weight and a 4-pound weight to weigh a 3-pound object, by placing the
  3-pound object and 1-pound weight on one side of the scale, and
  the 4-pound weight on the other side.
  """

  This is very slow:
  - broken-weignts (with minimization): about 23s
  - broken-weights (plain "assert"): about 36s
  
  Cf my Z3/Python model broken_weights.py (0.75s) and my Picat model 
  broken_weights.pi (0.08s).

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")
(require rosette/solver/smt/z3)

#|
  ((model
   [weights$0 1]
   [weights$1 3]
   [weights$2 9]
   [weights$3 27]
   ...

   Minimization takes 22.9s! It's actually faster than just solver-check (30s).

|#
(define (broken-weights)
  (displayln "broken-weights")
  (clear-vc!)
  
  (define m 40)
  (define n 4)
  
  (define solver (z3))
  (define constraints '())
  
  ; #:length must be a constant! Why?
  ; (define-symbolic weights integer? #:length n) ; "expected a natural? for #:length"
  ; (see broken-weights for another solution)
  (define-symbolic weights integer? #:length 4)

  ; domain: 1..m
  (for ([i n])
    (set! constraints (add constraints (<= 1 (list-ref weights i) m)))
    )

  ; Define a matrix of decision variables 40 x 4
  ; domain: -1..1
  (define-symbolic x integer? #:length 160)
  (for ([i (* 40 4)])
    (set! constraints (add constraints (<= -1 (list-ref x i) 1))))

  ; Constraints
  (set! constraints (add constraints (distinct? weights)))
  (set! constraints (add constraints (increasing-strict weights)))
  (set! constraints (add constraints (= (sum weights) m)))

  ; Ensure that all the 40 corresponding x-slices gives 1..40
  (for ([i1 (range 1 (add1 m))])
    (let* ([i (sub1 i1)]
           [x-slice (for/list ([j (range (* i n) (+ (* i n) 4))]) (list-ref x j))]
           [sp (scalar-product weights x-slice)])
      (set! constraints (add constraints (= i sp)))
      ))

 
  (solver-assert solver constraints)
  (solver-minimize solver (list (last weights)))
  (solver-check solver)
  
  )

; (time (broken-weights))


#|
  Using "plain" assert and solve

  It works, but
  - not optimality (it give the optimal solution anyway)
  ;; - very slow: about 36s! cpu time: 43 real time: 34401 gc time: 14
  - with QF_FD: cpu time: 46 real time: 555 gc time: 12

(weights (1 3 9 27))
(x)
  0  0  0  0
  1  0  0  0
 -1  1  0  0
  0  1  0  0
  1  1  0  0
 -1 -1  1  0
  0 -1  1  0
  1 -1  1  0
 -1  0  1  0
  0  0  1  0
  1  0  1  0
 -1  1  1  0
  0  1  1  0
  1  1  1  0
 -1 -1 -1  1
  0 -1 -1  1
  1 -1 -1  1
 -1  0 -1  1
  0  0 -1  1
  1  0 -1  1
 -1  1 -1  1
  0  1 -1  1
  1  1 -1  1
 -1 -1  0  1
  0 -1  0  1
  1 -1  0  1
 -1  0  0  1
  0  0  0  1
  1  0  0  1
 -1  1  0  1
  0  1  0  1
  1  1  0  1
 -1 -1  1  1
  0 -1  1  1
  1 -1  1  1
 -1  0  1  1
  0  0  1  1
  1  0  1  1
 -1  1  1  1
  0  1  1  1

|#
(define (broken-weights2)
  (displayln "\nbroken-weights2")
  (clear-vc!)

  (current-solver (z3 #:logic 'QF_FD))  
  
  (define m 40)
  (define n 4)
  
  (define solver (z3))
  (define constraints '())
  
  ; #:length must be a constant! Why?
  ; (define-symbolic weights integer? #:length n) ; "expected a natural? for #:length"
  (define weights (make-var-array-integer n 1 m))

  ; Define a matrix of decision variables m x n, domain: -1..1
  (define x (make-var-matrix-integer m n -1 1))

  ; Constraints
  (assert (all-different weights))
  (assert (increasing weights))
  (assert (= (sum weights) m))

  ; Ensure that all the 40 corresponding x-slices gives 1..40
  (for ([i (range 1 (add1 m))])
    (let ([sp (scalar-product weights (list-ref x (sub1 i)))])
      (assert (= i sp)
    )))

  (define sol (solve #t))
  (show "weights" (evaluate weights sol))
  (show "x")
  (show-matrix (for/list ([x-row x])
                 (evaluate x-row sol)))
  
  )

(time (broken-weights2))

