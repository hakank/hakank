#| 

  AoC utils in Racket.

  Advent of Code utilities

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))
(require math)

; Read a file with of space separated integers
(define (read-integer-file file)
  (map (lambda (v) (map string->number (string-split v))) (file->lines file)))

; Transpose a list of lists
(define (transpose x) (apply map list x))

; Sum a list of integers
(define (sum x) (apply + x))

; Converts a boolean to integer (0 or 1)
(define (b2i x) (if x 1 0))

; Wrapper for display
(define (show . a) (displayln a))
