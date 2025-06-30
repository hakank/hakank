#| 

  Who killed Agatha (The Dreadsbury Mansion Murder Mystery) in Racket/Rosette.

  This is a standard benchmark for theorem proving.
 
  http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html
  """ 
  Someone in Dreadsbury Mansion killed Aunt Agatha. 
  Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
  are the only ones to live there. A killer always hates, and is no 
  richer than his victim. Charles hates noone that Agatha hates. Agatha 
  hates everybody except the butler. The butler hates everyone not richer 
  than Aunt Agatha. The butler hates everyone whom Agatha hates. 
  Noone hates everyone. Who killed Agatha? 
  """

  Originally from F. J. Pelletier: 
  Seventy-five problems for testing automatic theorem provers. 
  Journal of Automated Reasoning, 2: 191 216, 1986.
  http://www.sfu.ca/~jeffpell/papers/75ATPproblems86.pdf

  All solutions indicates that Agatha (0) is the killer:
  '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (num-sols 64)

  Note: Most of my other constraint models gives just 8 solutions (all indicating Agatha as the killer).
        Not sure what I've missed here...

  I have blogged about the problem here:
  * "Learning constraint programming - part II: Modeling with the Element constraint"
    http://www.hakank.org/constraint_programming_blog/2009/05/learning_constraint_programmin.html
  * "Learning Constraint Programming IV: Logical constraints: Who killed Agatha? revisited"
  http://www.hakank.org/constraint_programming_blog/2009/05/learning_constraint_programmin_3.html

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")

(require rosette/solver/smt/z3)
(current-solver (z3 #:logic 'QF_FD))

(define n 3)
(define agatha 0)
(define butler 1)
(define charles 2)

(define-symbolic* killer integer?)
(assert (<= agatha killer charles))

; Hates relation
(define hates (for/list ([i n])
                (for/list ([j n])
                  (define-symbolic* hates integer?)
                  (assert (<= 0 hates 1))
                  hates)))

(define richer (for/list ([i n])
                 (for/list ([j n])
                   (define-symbolic* richer integer?)
                   (assert (<= 0 richer 1))
                   richer)))

;
; The constraints
;
;
; Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
; are the only ones to live there. 
;

; * A killer always hates, and is no richer than his victim. 
(assert (= (ix hates killer agatha) 1))
(assert (= (ix richer killer agatha) 0))

; * Define the concept of richer: no one is richer than him-/herself
(for ([i n]) (assert (= (ix richer i i) 0)))


#|
; (contd...) if i is richer than j then j is not richer than i
|#
(for ([i n]
      [j n]) (when (not (= i j)) (assert (<=> (= (ix richer i j) 1)
                                              (= (ix richer j i)) 0))))

; * Charles hates no one that Agatha hates.
(for ([i n]) (assert (=> (= (ix hates agatha i) 1)
                         (= (ix hates charles i) 0) )))

; * Agatha hates everybody except the butler. 
(assert (= (ix hates agatha butler) 0))
(assert (= (ix hates agatha charles) 1))
(assert (= (ix hates agatha agatha) 1))


; * The butler hates everyone not richer than Aunt Agatha. 
(for ([i n]) (assert (=> (= (ix richer i agatha) 0)
                         (= (ix hates butler i) 1))))

; * The butler hates everyone whom Agatha hates. 
(for ([i n]) (assert (=> (= (ix hates agatha i) 1)
                         (= (ix hates butler i) 1))))

; * No one hates every one.
(for ([i n]) (assert (<= (sum (for/list ([j n]) (ix hates i j))) 2)))

; * Who killed Agatha?


;; (define sol (solve #t))
;; sol
;; (evaluate killer sol)

; (show-all-solutions (list killer hates richer))
; (show-all-solutions killer)

(define sols (get-all-solutions killer))
sols
(show "num-sols" (length sols))
