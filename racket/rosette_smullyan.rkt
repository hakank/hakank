#| 

  Smullyan problems in Racket/Rosette.

  Some problem from Raymond Smullyan's great book
  "What is the name of this book? - The riddle of dracula and other logical puzzles".

  The problem below are ported from my Picat models smullyan_knights_knaves.pi

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")

(define knight #t)
(define knave #f)
  
(require rosette/solver/smt/z3)
(define solver (z3 #:logic 'QF_FD))

#| 
  Smullyan problem #26 

  B: A says he is a knave
  C: B is a knave
  What are B and C?
  Solution: A: unknown, B: knave, C: knight

(sol (model
 [a #f]
 [b #f]
 [c #t]))

(sol (model
 [a #t]
 [b #f]
 [c #f]))


|#
(define (smullyan26)
  (displayln "\nsmullyan26")
  (clear-vc!)

  (define-symbolic a b c boolean?)
  ; B: A says he is a knave
  (assume (<=> b (<=> a (eq? a knave))))
  ; C: B is a knave
  (assume (<=> c (eq? a knave)))
  ; (displayln (solve (list a b c)))
  (show-all-solutions (list a b c))
  ; (pretty-print (vc))
  
  )
(smullyan26)

#|
  Smullyan #27
  B: A said that there are exactly 1 knight
  C: B is a knave      
  What are B and C
  Solution: B: knave, C: knight

(sol (model
 [b #f]
 [c #t]))

|#
(define (smullyan27)
  (displayln "\nsmullyan27")
  (clear-vc!)

  (define-symbolic a b c boolean?)
  
  ; B: A says he is a knave
  (assume (<=> b (<=> a (eq? a knave))))
  ; C: B is a knave
  (assume (<=> c (eq? b knave)))
  ; (displayln (solve (list a b c)))
  ; (displayln (complete-solution (solve (list a b)) (list a b)))
  (show-all-solutions (list a b c))  
  ; (pretty-print (vc))

  )
(smullyan27)


#|
  Smullyan #28
  Just a and b
  A: at least one of us is a knave
  Solution: A: knight, B: knave

(sol (model
 [a #t]
 [b #f]))

|#
(define (smullyan28)
  (displayln "\nsmullyan28")
  (clear-vc!)

  (define-symbolic a b boolean?)

  ; A: at least one of us is a knave
  (assume (<=> a (or (eq? a knave) (eq? b knave))))
  ; (displayln (solve (list a b)))
  (show-all-solutions (list a b))  
  ;(pretty-print (vc))

  )

(smullyan28)

#|
  Smullyan #29
  A: either A is a knave or B is a knight
  Solution: A: knight, B: knight

(sol (model
 [a #t]
 [b #t]))


|#
(define (smullyan29)
  (displayln "\nsmullyan29")
  (clear-vc!)

  (define-symbolic a b boolean?)

  ; A: either A is a knave or B is a knight
  (assume (<=> a (|| (eq? a knave) (eq? b knight))))
  
  ; (displayln (solve (list a b)))
  (show-all-solutions (list a b))  
  ; (pretty-print (vc))
  )
(smullyan29)


#|
  Problem 30
  A: either A is a knave or 2 + 2 = 5
  Solution: Infeasible.

  [assume] failed

  (Thus it's not run. TODO: Fix that!)

|#
(define (smullyan30)
  (displayln "\nsmullyan30")
  (clear-vc!)

  (define knight #t)
  (define knave #f)
  
  (define-symbolic a b boolean?)

  ; A: either A is a knave or B is a knight
  (assume (<=> a (or (eq? a knave) (= (+ 2 2) 5))))

  ; (displayln (solve (list a b)))
  (show-all-solutions (list a b)) 
  ; (pretty-print (vc))
  )
; (smullyan30)


#|
  Problem #31
  A: All are knaves
  B: Exactly one is a knight
  Solution: A: knave, B: knight, C: knave

(sol (model
 [a #f]
 [b #t]
 [c #f]))

|#
(define (smullyan31)
  (displayln "\nsmullyan31")
  (clear-vc!)

  (define-symbolic a b c boolean?)

  ; A: All are knaves
  (assume (<=> a (&& (eq? a knave) (eq? b knave) (eq? c knave))))
  ; B: Exactly one is a knight
  (assume (<=> b (|| (and (eq? a knight) (eq? b knave) (eq? c knave))
                     (and (eq? a knave) (eq? b knight) (eq? c knave))
                     (and (eq? a knave) (eq? b knave) (eq? c knight)))))

  ; (displayln (solve (list a b)))
  (show-all-solutions (list a b c)) 
  ; (pretty-print (vc))
  )
(smullyan31)


#|
  Problem #32

  A: all are knaves
  B: exactly one is a knave
  Solution: A: knave, B: knight or knave, C: knight

(sol (model
 [a #f]
 [b #f]
 [c #t]))

(sol (model
 [a #f]
 [b #t]
 [c #t]))


|#
(define (smullyan32)
  (displayln "\nsmullyan32")
  (clear-vc!)

  (define-symbolic a b c boolean?)

  ; A: All are knaves
  (assume (<=> a (&& (eq? a knave) (eq? b knave) (eq? c knave))))
  ; B: Exactly one is a knave
  (assume (<=> b (|| (and (eq? a knave) (eq? b knight) (eq? c knight))
                     (and (eq? a knight) (eq? b knave) (eq? c knight))
                     (and (eq? a knight) (eq? b knight) (eq? c knave)))))

  (show-all-solutions (list a b c)) 
  ; (displayln (solve (list a b)))
  ; (pretty-print (vc))
  )
(smullyan32)

#|
  Problem #33
  A: A is knave, B is knight.
  Solutions: A: knave, B: knave

(sol (model
 [a #f]
 [b #f]))

|#
(define (smullyan33)
  (displayln "\nsmullyan33")
  (clear-vc!)

  (define-symbolic a b boolean?)

  ; A: A is knave, B is knight.
  (assume (<=> a (&& (eq? a knave) (eq? b knight))))

  (show-all-solutions (list a b)) 
  ; (displayln (solve (list a b)))
  ; (pretty-print (vc))
  )
(smullyan33)

#|
  Problem 34
  A: B is a knave
  B: A and C is of the same type

  Solution: A and B is not of the same type, C: knave

(sol (model
 [a #t]
 [b #f]
 [c #f]))

(sol (model
 [a #f]
 [b #t]
 [c #f]))


|#
(define (smullyan34)
  (displayln "\nsmullyan34")
  (clear-vc!)

  (define-symbolic a b c boolean?)
  
  ; A: B is a knave
  (assume (<=> a (&& (eq? b knave))))
  
  ; B: A and C is of the same type
  (assume (<=> b (eq? a c)))

  (show-all-solutions (list a b c))  
  ; (pretty-print (vc))
  )
(smullyan34)


;
; Some related problems
;

#|
  p_extra1

  A says: We are both knaves (i.e. both lies)
  Solution: A is a knave B is a knight

(sol (model
 [a #f]
 [b #t]))

|#
(define (p_extra1)
  (displayln "\nsp_extra1")
  (clear-vc!)

  (define-symbolic a b boolean?)
  
  ; A: We are both knaves (i.e. both lies)
  (assume (<=> a (&& (eq? a knave) (eq? b knave))))
  
  (show-all-solutions (list a b))
  
  ; (pretty-print (vc))
  )
(p_extra1)

#|
  p_extra2

  A says: We are both knights (i.e. both tells truth)
  B says: A is a knave
  Solution: A is a knave B is a knight

(sol (model
 [a #f]
 [b #t]))

|#
(define (p_extra2)
  (displayln "\nsp_extra2")
  (clear-vc!)

  (define-symbolic a b boolean?)
  
  ; A says: We are both knights (i.e. both tells truth)
  (assume (<=> a (&& (eq? a knight) (eq? b knight))))
  ; B says: A is a knave
  (assume (<=> b (&& (eq? a knave))))
  
  (show-all-solutions (list a b))
  
  ; (pretty-print (vc))
  )
(p_extra2)

