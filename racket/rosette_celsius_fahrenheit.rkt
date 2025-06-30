#| 

  Celsius <-> Fahrenheit conversion in Racket/Rosette.

  Fahrenheiht = Celsium * 1.8 + 32

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")
; (require rosette/solver/smt/z3)

(define-symbolic c real?)
(define-symbolic f real?)

(assert (= f (+ (* c 1.8) 32)))

(for ([v (list c f)])
  (assert (<= -300 v 300)))

; Some tests
; (assert (= c 100))
; (assert (or (= c 0) (= f 0)))

(define sol (solve #t))
(evaluate (list c f) sol)

(show-all-solutions (list c f))
