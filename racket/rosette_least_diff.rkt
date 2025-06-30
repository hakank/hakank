#| 

  Least diff problem in Racket/Rosette.

  The model solves the following problem:
  
  What is the smallest difference between two numbers X - Y
  if you must use all the digits (0..9) exactly once, i.e.
  Minimize the difference 
    ABCDE - FGHIJ

  Solution:
  ((5 0 1 2 3 4 9 8 7 6 50123 49876 247))
  50123 - 49876 = 247

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")

(require rosette/solver/smt/z3)

; cpu time: 29 real time: 187 gc time: 0
(current-solver (z3 #:logic 'QF_LIA))

; slower:
; cpu time: 45 real time: 466 gc time: 0
; (current-solver (z3 #:logic 'QF_FD)) ; slower

(define (least-diff)

  (clear-vc!)
  
  ; (output-smt "smt_log.txt")
  
  (define-symbolic* a integer?)
  (define-symbolic* b integer?)
  (define-symbolic* c integer?)
  (define-symbolic* d integer?)
  (define-symbolic* e integer?)

  (define-symbolic* f integer?)
  (define-symbolic* g integer?)
  (define-symbolic* h integer?)
  (define-symbolic* i integer?)
  (define-symbolic* j integer?)

  (define digits (list a b c d e f g h i j))
  (for ([v digits])
    (assert (<= 0 v 9)))

  (assert (all-different digits))

  (assert (all-different (list a b c d e)))
  (assert (all-different (list f g h i j)))

  ; (assert (> a 0))
  ; (assert (> f 0))

  (define-symbolic* x integer?)
  (define-symbolic* y integer?)
  (define-symbolic* opt integer?) ; the difference

  (for ([v (list x y)])
    (assert (<= 10000 v 99999)))
  (assert (<= 0 opt 99999))

  (assert (= x (scalar-product (list a b c d e) (list 10000 1000 100 10 1))))
  (assert (= y (scalar-product (list f g h i j) (list 10000 1000 100 10 1))))
  ; (assert (= x (+ (* a 10000) (* b 1000) (* c 100) (* d 10) e)))
  ; (assert (= y (+ (* f 10000) (* g 1000) (* h 100) (* i 10) j)))

  (assert (= opt (- x y)))
  (assert (>= opt 0))


  (define sol (first (get-min-solution (flatten (list digits x y opt)) opt #:debug? #f)))
  (show sol)
  (displayln (format "~a - ~a = ~a" (ix sol 10) (ix sol 11) (ix sol 12)))
  )

(time (least-diff))
  



