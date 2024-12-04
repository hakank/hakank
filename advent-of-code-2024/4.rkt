#| 

  Advent of Code Day 2024 Day 4 in Racket.

  https://adventofcode.com/2024/day/4


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

; See http://www.hakank.org/advent-of-code-2024/aoc_utils.rkt
(require "aoc_utils.rkt")

(define file "4.txt")

(define m (map string->list (file->lines file)))
(define t (transpose m) )

(define (xmas1 s line)
  (+ (count-occurrences-sublist s line)
     (count-occurrences-sublist s (reverse line))))

(define (part1)
  (let ([XMAS (string->list "XMAS")])
    (+ (for/sum ([line m]) (xmas1 XMAS line))
       (for/sum ([line t]) (xmas1 XMAS line))
       (for/sum ([line (all-diagonals m)]) (if (>= (length line) 4) (xmas1 XMAS line) 0)))))

(part1)

(define (mas2 m)
  (let ([n (length m)])
    (for*/sum ([i (range 1 (sub1 n))]
                [j (range 1 (sub1 n))]
                #:when (and
                        (eq? #\M (list-ref-2d m (- i 1) (- j 1)))
                        (eq? #\S (list-ref-2d m (- i 1) (+ j 1)))
                        (eq? #\A (list-ref-2d m i j))
                        (eq? #\M (list-ref-2d m (+ i 1) (- j 1)))
                        (eq? #\S (list-ref-2d m (+ i 1) (+ j 1)))
                        ))
                1)))
                        
(define (part2)
  (let ([m2 (reverse (map reverse m))]
        [t2 (reverse (map reverse t))])
    (+ (mas2 m) (mas2 m2) (mas2 t) (mas2 t2))
    
    ))

(part2)
