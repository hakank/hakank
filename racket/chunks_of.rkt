#| 

  Chunks of function in Racket.

  (chunks-of a n #:partition? [partition? #f])
  Returns chunks of length n for a list a.
  There are two modes:
  * partition? #t (default)
    Returns a list of partitions in the list a of length n 
  * partition? #f
    Returns a list of sublist (overlapping) in the list a of length n 

  Note: For the partiton mode, the last chunk may be of length < n.

  Examples:
  > (chunks-of (range 11) 3)
  ((0 1 2) (3 4 5) (6 7 8) (9 10))
  > (chunks-of (range 11) 3 #:partition? #f)
  -> ((0 1 2) (1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8) (7 8 9) (8 9 10)))

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))
(require (except-in math/number-theory permutations))
;;; (require racket/trace)
(require "utils_hakank.rkt")

#|
  Output:
  (all sublists ((0 1 2) (1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8) (7 8 9) (8 9 10)))
  (partition ((0 1 2) (3 4 5) (6 7 8) (9 10)))

|#
(show "all sublists" (chunks-of (range 11) 3))
(show "partition" (chunks-of (range 11) 3 #:partition? #t))
