#| 

  Advent of code 2024 day 2 in Racket.

  https://adventofcode.com/2024/day/2

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

; (require math)
(require "aoc_utils.rkt")

(define file "2.txt")

(define lines (read-integer-file file))

; Is this line safe?
(define (safe line)
  (let* ([s (sort line <)]
         [t (differences s)])
    (and
     (or (equal? line s) (equal? line (reverse s))) ; increasing or descrasing?
     (>= (min-list t) 1)
     (<= (max-list t) 3)
     )))

(define (part1)
  (for/sum ([line lines]
            #:when (safe line))
                  1))

(define (part2)
  (for/sum ([line lines]
            #:when (or (safe line)
                       ; Check if the line is safe if one number is removed
                       (> (for/sum ([i (length line)])
                            (b2i (safe (remove-index i line)))) 0)))
    1))

(part1)
(part2)
