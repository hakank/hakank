#| 

  Advent of Code 2024 Day 13 in Racket.

  https://adventofcode.com/2024/day/13

  Using math/matrix/matrix-solve

  (part 1)
  cpu time: 32 real time: 32 gc time: 5
  40069
  (part 2)
  cpu time: 54 real time: 54 gc time: 27
  71493195288102
  racket 13_matrix.rkt  0,77s user 0,13s system 99% cpu 0,900 total

  Cf 13.rkt (using Rosette) which took 4.406s

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(require "aoc_utils.rkt")

(require math/matrix)
(require math/array)

(define (s ax ay bx by px py)
  (define sol (matrix-solve (matrix [[ax bx] [ay by]]) (col-matrix [px py])))
  (let-values ([(x y) (apply values (array->list sol))])
    (if (and (integer? x) (integer? y))
        (+ (* x 3) y)
        '()))
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
                             (s ax ay bx by px py)
                             (s ax ay bx by (+ px extra) (+ py extra))
                             )
                         )))))))

