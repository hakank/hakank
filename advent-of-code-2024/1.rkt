#| 

  Advent of code 2024 day 1 in Racket.

  https://adventofcode.com/2024/day/1

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

; See http://www.hakank.org/advent-of-code-2024/aoc_utils.rkt
(require "aoc_utils.rkt")

(define file "1.txt")

(define contents (read-integer-file file))
(let-values ([(a b) (apply values (transpose contents))])
  
  ; Part 1
  (let ([as (sort a <)]
        [bs (sort b <)])
    
    (show "Part 1:" (sum (map abs (map - as bs)))))
  
  ; Part 2
  (show "Part 2:" (for*/sum ([x a]
                             [y b])
                    (* x (b2i (= x y)))))
  
  )
    

  

