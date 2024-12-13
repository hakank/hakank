#| 

  Advent of Code 2024 Day 13 in Racket/Rosette.

  https://adventofcode.com/2024/day/13

  (part 1)
  cpu time: 444 real time: 1276 gc time: 31
  40069
  (part 2)
  cpu time: 452 real time: 1377 gc time: 50
  71493195288102
  racket -y 13.rkt  2,24s user 0,41s system 60% cpu 4,406 total


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(require "aoc_utils.rkt")

(require rosette/solver/smt/z3)
; (current-solver (z3 #:logic 'QF_FD)) ; Slower
(current-solver (z3 #:logic 'QF_LIA)) ; Total 4.486s


(define (s ax ay bx by px py part)
  (clear-vc!)
  (define-symbolic* x integer?)
  (define-symbolic* y integer?)
  (for ([v (list x y)])
    (if (= part 1)
        (assert (<= 1 v 100))
        (assert (<= 1 v (expt 10 12))))
    )

  (assert (= px (+ (* ax x) (* bx y))))
  (assert (= py (+ (* ay x) (* by y))))
  (define sol (solve #t))
  (if (not (equal? sol (unsat)))
      (evaluate (+ (* x 3) y) sol)
      '())  
  )

(define file "13.txt")
(define content (string-split (file->string file) #px"\n\n"))
(define regex "Button A: X\\+(\\d+), Y\\+(\\d+)\nButton B: X\\+(\\d+), Y\\+(\\d+)\nPrize: X=(\\d+), Y=(\\d+)")

(define extra 10000000000000)
(for ([part '(1 2)])
  (show "part" part)
  (displayln (time (sum (filter number?
                     (for/list ([c content])
                       (let-values ([(ax ay bx by px py)
                                     (apply values (map string->number (rest (regexp-match (pregexp regex) c))))])
                         (if (= part 1)
                             (s ax ay bx by px py part)
                             (s ax ay bx by (+ px extra) (+ py extra) part)
                             )
                         )))))))

