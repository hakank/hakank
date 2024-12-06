#| 

  Advent of Code Day 2024 Day 5 in Racket.

  https://adventofcode.com/2024/day/5

  Part 2, using Rosette

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

; Gives strange error when defining xs in part2-model:
;   constant: expected a solvable type, given #<procedure:integer?>
; (require racket) 

; See http://www.hakank.org/advent-of-code-2024/aoc_utils.rkt
(require "aoc_utils.rkt")

(require rosette/solver/smt/z3)
; (require rosette/solver/smt/cvc4) ; Error 
; (require rosette/solver/smt/cvc5)


; (current-solver (z3 #:logic 'QF_FD)) ; Solution -> (unknown)
(current-solver (z3))
; (current-solver (cvc5 #:logic 'QF_NIA))
; (current-solver (cvc4))

; Max page number is 99
(current-bitwidth 8)

(define file "5.txt")

(define (split-split s sep)
  (map (lambda (p) (map string->number (string-split p sep))) (string-split s "\n")))

(define (get-mid a)
  (let ([n (length a)])
    (list-ref a (floor (/ n 2)))))

(define (all-different x) (apply distinct? x))

; Using Rosette SMT solver
(define (part2-model prec cs)
  (clear-vc!) ; Clear previous models
  
  (define n (length cs))
  (define cs2 (sort cs <))
  (show "n" n)
  (define xs (for/list ([i n])
              (define-symbolic* x integer?)
              ; (assert (member x cs)) 
              (assert (or (for/list ([c cs]) (= x c))))
              x))
  (assert (all-different xs))
  (for ([p prec])
    (define a (first p))
    (define b (second p))
    ; Ensure that the positions in x satisfies the precedences
    (when (and (member a cs) (member b cs))
      (define-symbolic* pa integer?)
      (define-symbolic* pb integer?)
      (assert (<= 0 pa n))
      (assert (<= 0 pb n))
      (assert (= a (list-ref xs pa))) ; xs[pa] = a 
      (assert (= b (list-ref xs pb))) ; xs[pb] = b
      (assert (< pa pb)) 
      )
    )
  (define sol (solve #t))
  (show "xs" (evaluate xs sol))
  (evaluate xs sol)
  )


(define input (file->string file))
(let-values ([(prec1 orders1) (apply values (string-split input "\n\n"))])
  (let ([prec (split-split prec1 "|")]
        [orders (split-split orders1 ",")]
        [h (make-hash)])

    ; Create a map of the precedences
    (for ([ab prec])
      (let ([a (first ab)]
            [b (second ab)])
        (hash-set! h a (append (hash-ref h a '()) (list b)))
        ))
    
    (define incorrect '()) ; For part 2
    ; Part 1, and collect data (the incorrect list) for part 2)
    (for ([order orders])
      (define ok #t)
      (for ([i (range (length order))])
        (define t (hash-ref h (list-ref order i) '()))
        (for ([j (range (add1 i) (length order))])
          (when (not (and ok (member (list-ref order j) t)))
            (set! ok #f))
          ))
      (when (not ok)
        (set! incorrect (append incorrect (list order))))
      )
    
    (for/sum ([cs incorrect])
      (let ([x (part2-model prec cs)])
        (show "mid" (get-mid x))
        (get-mid x))
      )
    ))

