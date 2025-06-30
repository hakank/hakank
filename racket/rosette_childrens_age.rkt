#| 

  Children's Age problem in Racket/Rosette.

  From https://javascript.plainenglish.io/youre-a-good-programmer-if-you-can-solve-this-problem-b3bfe66a962a
  """
  If you can solve this problem and understand the dialogue between two programmers, 
  you either work in a large IT company or should consider going there and surprising everyone.

  Once upon a time, two programmers who had not spoken for many years met. One asked the other:

  - They say you have children?
  - Yes, three sons.
  - And how old are they?
  - In total — 13.
  - What else can you say about them?
  - If you multiply their ages, you get the same number as the number of windows in 
    that house over there.

  Having counted the windows of the house, the first one answers:

  - Great, but that’s not enough to answer.
  - I can add that the eldest son is red-haired.
  - Oh, well now everything is clear, that means, your sons (and then follows an 
    answer in which the ages of each of the three children are named).

  How old were the children? And most importantly, how was the first one able to figure 
  out their age from this seemingly illogical dialogue?
  """

  The unique solution: The ages are 2, 2, and 9.

  Here's the run of the model:
  (h #hash((c . 11) (p . 11) (a . 1) (b . 1)))
  (h #hash((c . 9) (p . 27) (a . 1) (b . 3)))
  (h #hash((c . 9) (p . 36) (a . 2) (b . 2)))
  (h #hash((c . 10) (p . 20) (a . 1) (b . 2)))
  (h #hash((c . 6) (p . 72) (a . 3) (b . 4)))
  (h #hash((c . 8) (p . 32) (a . 1) (b . 4)))
  (h #hash((c . 8) (p . 48) (a . 2) (b . 3)))
  (h #hash((c . 6) (p . 60) (a . 2) (b . 5)))
  (h #hash((c . 5) (p . 80) (a . 4) (b . 4)))
  (h #hash((c . 6) (p . 36) (a . 1) (b . 6)))
  (h #hash((c . 7) (p . 35) (a . 1) (b . 5)))
  (h #hash((c . 7) (p . 56) (a . 2) (b . 4)))
  (h #hash((c . 5) (p . 75) (a . 3) (b . 5)))
  (h #hash((c . 7) (p . 63) (a . 3) (b . 3)))
  solution:
  (a 2 b 2 c 9)

  It works but is very slow: about 10s.
  Later: with the QF_FD solver: 0.8s

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")
(require rosette/solver/smt/z3)

(current-solver (z3 #:logic 'QF_FD))  

(define (childrens-age) 
  ; """
  ; - They say you have children?
  ; - Yes, three sons.
  ; """
  (define-symbolic a b c p integer?)
  (assume (<= 1 a 13))
  (assume (<= 1 b 13))
  (assume (<= 1 c 13))
  (assume (<= 1 p (expt 13 2)))
  
  ; Symmetry breaking
  (assume (<= a b c))
  
  ; """
  ; - And how old are they?
  ; - In total — 13.
  ; - What else can you say about them?
  ; - If you multiply their ages, you get the same number as the number of windows in 
  ;   that house over there.
  ; """
  (assume (= (+ a b c) 13))
  ; The product 
  (assume (= (* a b c) p))
  
  ; Get all solutions
  (define all-sols (get-all-solutions (list a b c p) #:debug? #t))

  ; """
  ; Having counted the windows of the house, the first one answers:
  ;
  ; - Great, but that’s not enough to answer.
  ; """
  ; - I can add that the eldest son is red-haired.
  ; - Oh, well now everything is clear, that means, your sons (and then follows an 
  ;   answer in which the ages of each of the three children are named).
  ; """
  
  ; Since the addition (13) and product (M) did not give a unique answer, we
  ; search for the answers that has the same product: 36.
  ; We are then looking for a solution where the eldest (c) is older than b,
  ; i.e. not a twin/triplet.
  
  ; Collect the candidates with the same product
  (define h (make-hash))
  (for ([sol all-sols])
    (let ([v (last sol)])
      (hash-set! h v (append (hash-ref h v '()) (list sol)))
      ))
  (show "solution:")
  
  ; Look for the product with more than one candidatess
  ; And check for the solution were the oldest age is unique
  (for ([key (hash-keys h)])
    (let ([v (hash-ref h key)])
      (when (> (length v) 1)
        (for ([hh v])
        (let ([a-val (first hh)]
              [b-val (second hh)]
              [c-val (third hh)])
          ; Ensure that the oldest is not a twin/triplet
          (when (and (> c-val a-val) (> c-val b-val))
            (show "a" a-val "b" b-val "c" c-val))
          )
        ))
      
      ))
  )
(childrens-age)

