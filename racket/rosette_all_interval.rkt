#| 

  All interval problem in Racket/Rosette.

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c , d, ...), represented by 
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
  once and in which the musical intervals between neighbouring notes cover 
  the full set of intervals from the minor second (1 semitone) to the major 
  seventh (11 semitones). That is, for each of the intervals, there is a 
  pair of neigbhouring pitch-classes in the series, between which this 
  interval appears. The problem of finding such a series can be easily 
  formulated as an instance of a more general arithmetic problem on Z_n, 
  the set of integer residues modulo n. Given n in N, find a vector 
  s = (s_1, ..., s_n), such that (i) s is a permutation of 
  Z_n = {0,1,...,n-1}; and (ii) the interval vector 
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
  called an all-interval series of size n; the problem of finding such 
  a series is the all-interval series problem of size n. We may also be 
  interested in finding all possible series of a given size. 
  """

  All 8 solutions for n=6
  ((2 3 1 4 0 5) (1 2 3 4 5))
  ((2 1 4 0 5 3) (1 3 4 5 2))
  ((1 5 0 3 4 2) (4 5 3 1 2))
  ((1 4 0 5 3 2) (3 4 5 2 1))
  ((1 5 0 3 2 4) (4 5 3 1 2))
  ((3 0 5 1 2 4) (3 5 4 1 2))
  ((2 0 5 1 4 3) (2 5 4 3 1))
  ((3 2 0 5 1 4) (1 2 5 4 3))


  Timing for n=4..10:
  (n 4)
  (num-solutions 1)
  cpu time: 4 real time: 20 gc time: 0

  (n 5)
  (num-solutions 3)
  cpu time: 13 real time: 56 gc time: 0

  (n 6)
  (num-solutions 8)
  cpu time: 37 real time: 198 gc time: 0

  (n 7)
  (num-solutions 9)
  cpu time: 53 real time: 477 gc time: 0

  (n 8)
  (num-solutions 15)
  cpu time: 118 real time: 1965 gc time: 8

  (n 9)
  (num-solutions 42)
  cpu time: 425 real time: 11566 gc time: 43

  (n 10)
  (num-solutions 104)
  cpu time: 1421 real time: 181196 gc time: 109


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(require "rosette_utils.rkt")

(require rosette/solver/smt/z3)

; (current-solver (z3 #:logic 'QF_FD)) ; No solution. Why?

(define (all-interval n)
  (clear-vc!)
  (define n1 (sub1 n))

  (define sum-distinct (/ (* n (add1 n)) 2))

  (define x (for/list ([i n])
              (define-symbolic* x integer?)
              (assert (<= 0 x n1))
              x))

  (define diff (for/list ([i n1])
                 (define-symbolic* diff integer?)
                 (assert (<= 0 diff n1))
                 diff))


  (assert (all-different x))
  (assert (all-different diff))

  (for ([k n1])
    (assert (= (ix diff k) (abs (- (ix x (+ k 1)) (ix x k))))))

  ; symmetry breaking
  (assert (< (ix x 0) (ix x n1)))
  (assert (< (ix diff 0) (ix diff 1)))

  (define sol (get-all-solutions (list x diff)))
  (show "num-solutions" (length sol))
  sol

  )

(for ([sol (all-interval 6)])
  (displayln sol))

(newline)
(for ([n (range 4 11)])
  (show "n" n)
  (time (all-interval n))
  (newline)
  )
