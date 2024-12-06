#| 

  Advent of Code Day 2024 Day 5 in Racket.

  https://adventofcode.com/2024/day/5

  Part 1

  For part 2, see 5_part2_rosette.rkt (but it's too slow)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

; See http://www.hakank.org/advent-of-code-2024/aoc_utils.rkt
(require "aoc_utils.rkt")

(define file "5.txt")

(define (split-split s sep)
  (map (lambda (p) (map string->number (string-split p sep))) (string-split s "\n")))

(define (get-mid a)
  (let ([n (length a)])
    (list-ref a (floor (/ n 2)))))


(define input (file->string file))
(let-values ([(prec1 orders1) (apply values (string-split input "\n\n"))])
  (let ([prec (split-split prec1 "|")]
        [orders (split-split orders1 ",")]
        [h (make-hash)])

  (for ([ab prec])
    (let ([a (first ab)]
          [b (second ab)])
    (hash-set! h a (append (hash-ref h a '()) (list b)))
    ))
  (define incorrect '())
  (define part1
    (for/sum ([order orders])
      (define ok #t)
      (for ([i (range (length order))])
        (define t (hash-ref h (list-ref order i) '()))
        (for ([j (range (add1 i) (length order))])
          (when (not (and ok (member (list-ref order j) t)))
              (set! ok #f)
              )
          )
        )
      (if ok 
          (get-mid order)
          0)      
    ))
  (show "part1" part1)
  ))

