#| 

  SEND+MORE=MONEY in Racket/Rosette.

  Search a solution with distinct values for each variables
  that satisfies:
    SEND
    MORE
  + ====
   MONEY 

  

  The unique solution:
  '("sol" (model
   [s 9]
   [e 5]
   [n 6]
   [d 7]
   [m 1]
   [o 0]
   [r 8]
   [y 2]))

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")

(require rosette/solver/smt/z3)

(current-solver (z3 #:logic 'QF_FD))


(define-symbolic s e n d m o r y integer?)

(define sendmory (list s e n d m o r y))

; domains
;; (assume (<= 0 s 9))
;; (assume (<= 0 e 9))
;; (assume (<= 0 n 9))
;; (assume (<= 0 d 9))
;; (assume (<= 0 m 9))
;; (assume (<= 0 o 9))
;; (assume (<= 0 r 9))
;; (assume (<= 0 y 9))
(for ([v sendmory])
  (assume (<= 0 v 9)))

(assume (> s 0))
(assume (> m 0))        

; (assume (distinct? s e n d m o r y))
; (assume (apply distinct? sendmory))
(assume (all-different sendmory))

(assume (=
         (+
          (+            (* 1000 s) (* 100 e) (* 10 n) d)
          (+            (* 1000 m) (* 100 o) (* 10 r) e))
         (+ (* 10000 m) (* 1000 o) (* 100 n) (* 10 e) y)))

(time (show-all-solutions sendmory #:debug? #f))


         
