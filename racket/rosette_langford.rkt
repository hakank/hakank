#| 

  Langford's number problem in Racket/Rosette.

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  
  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html
  
  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552


  Note: For k=4 there are two different solutions:
     solution:[4,1,3,1,2,4,3,2]
     position:[2,5,3,1,4,8,7,6]
  and
     solution:[2,3,4,2,1,3,1,4]
     position:[5,1,2,3,7,4,6,8]

  With this symmetry breaking

     Solution[1] #< Solution[K2],

  then just the second solution is shown.

  Note: There are only solutions when K mod 4 == 0 or K mod 4 == 3.

  Some examples:

  Langford 4 (without symmetry breaking):
  '(((1 4 2 0 3 7 6 5)) ((4 0 1 2 6 3 5 7)))

  Langford n=4..8 with symmetry breaking:
  (n 4)
  cpu time: 18 real time: 52 gc time: 5
  (num-sols: 1)
  ((4 0 1 2 6 3 5 7))

  (n 5)
  k must be 0 or 3 modulo 4. k (5) modulo 0 is 1
  cpu time: 0 real time: 0 gc time: 0
  (num-sols: 0)

  (n 6)
  k must be 0 or 3 modulo 4. k (6) modulo 0 is 2
  cpu time: 0 real time: 0 gc time: 0
  (num-sols: 0)

  (n 7)
  cpu time: 403 real time: 2336 gc time: 59
  (num-sols: 26)
  ((2 9 6 0 7 1 3 4 12 10 5 13 8 11))
  ((9 0 2 7 4 1 5 11 3 6 12 10 8 13))
  ((1 8 6 0 7 2 4 3 11 10 5 13 9 12))
  ((7 0 1 8 6 4 2 9 3 5 13 12 11 10))
  ((3 9 0 6 7 1 2 5 12 4 11 13 8 10))
  ((9 3 0 8 1 5 2 11 6 4 13 7 12 10))
  ((0 3 7 8 4 5 1 2 6 11 13 10 12 9))
  ((8 0 1 6 7 2 4 10 3 5 11 13 9 12))
  ((0 9 4 5 1 6 3 2 12 8 10 7 13 11))
  ((1 6 8 0 7 4 2 3 9 12 5 13 11 10))
  ((0 8 5 1 7 3 4 2 11 9 6 13 10 12))
  ((4 0 9 7 5 1 2 6 3 13 12 11 8 10))
  ((0 4 9 5 6 1 3 2 7 13 10 12 8 11))
  ((0 3 8 5 7 4 1 2 6 12 10 13 11 9))
  ((9 0 4 1 7 5 2 11 3 8 6 13 12 10))
  ((10 0 4 6 1 2 5 12 3 8 11 7 9 13))
  ((9 0 2 5 7 1 4 11 3 6 10 13 8 12))
  ((10 5 0 1 7 2 3 12 8 4 6 13 9 11))
  ((0 7 8 1 3 4 5 2 10 12 6 9 11 13))
  ((9 1 8 2 0 3 5 11 4 12 7 6 10 13))
  ((10 0 4 2 5 6 1 12 3 8 7 11 13 9))
  ((10 1 7 3 0 2 5 12 4 11 8 6 9 13))
  ((0 9 3 6 4 1 5 2 12 7 11 10 8 13))
  ((0 5 9 6 1 3 4 2 8 13 11 7 10 12))
  ((0 9 6 3 1 4 5 2 12 10 8 7 11 13))
  ((0 9 4 6 1 3 5 2 12 8 11 7 10 13))

  (n 8)
  cpu time: 3112 real time: 21225 gc time: 124
  (num-sols: 150)
  ((12 10 1 2 3 4 0 6 14 13 5 7 9 11 8 15))
  ((2 0 9 7 5 8 6 1 4 3 13 12 11 15 14 10))
  ((10 1 9 2 0 8 3 5 12 4 13 7 6 15 11 14))
  ((0 4 9 10 6 1 3 5 2 7 13 15 12 8 11 14))
  ((10 0 1 6 8 2 7 4 12 3 5 11 14 9 15 13))
  ((11 0 2 4 8 5 7 1 13 3 6 9 14 12 15 10))
  ((0 4 10 6 9 1 5 3 2 7 14 11 15 8 13 12))
  ((1 6 10 0 7 8 4 2 3 9 14 5 13 15 12 11))
  ...
  ((0 10 4 9 1 5 3 6 2 13 8 14 7 12 11 15))
  ((0 11 5 8 1 3 4 6 2 14 9 13 7 10 12 15))
  ((4 0 10 7 9 1 5 2 6 3 14 12 15 8 13 11))
  ((0 11 6 4 1 8 5 3 2 14 10 9 7 15 13 12))
  ((1 11 9 2 4 5 0 6 3 14 13 7 10 12 8 15))
  ((11 1 8 0 3 7 2 6 13 4 12 5 9 14 10 15))
  ((0 8 1 9 7 3 4 6 2 11 5 14 13 10 12 15))
  ((9 0 2 10 7 1 4 5 11 3 6 15 13 8 12 14))
  ((0 11 8 4 1 3 5 6 2 14 12 9 7 10 13 15))



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(require "rosette_utils.rkt")

(require rosette/solver/smt/z3)
(current-solver (z3 #:logic 'QF_FD))

(define (langford1 k #:symmetry-breaking? [symmetry-breaking? #t])
  (define k2 (* 2 k))
  
  (define position (for/list ([i k2])
                     (define-symbolic* position integer?)
                     (assert (<= 0 position (sub1 k2)))
                     position))
  
  (define solution (for/list ([i k2])
                     (define-symbolic* solution integer?)
                     (assert (<= 1 solution k))
                     solution))
  
  (assert (all-different position))
  ; symmetry breaking
  (when symmetry-breaking?
    (assert (< (ix solution 0) (ix solution (sub1 k2)))))
  
  (for ([i (range 1 (add1 k))])
    (assert (= (ix position (+ i k -1)) (+ (ix position (sub1 i)) i 1)))
    (assert (= i (ix solution (ix position (sub1 i)))))
    (assert (= i (ix solution (ix position (+ k i -1)))))
    )
  
  (get-all-solutions (list position))    
  
  )

(define (langford k #:symmetry-breaking? [symmetry-breaking? #t] )
  (clear-vc!)

  (if (or (= (modulo k 4) 0) (= (modulo k 4) 3))
      (langford1 k #:symmetry-breaking? symmetry-breaking?)
      (begin
        (displayln (format "k must be 0 or 3 modulo 4. k (~a) modulo 0 is ~a" k (modulo k 4)))
        '()
        ))
      
  )

(displayln "Langford 4 without symmetry breaking:")
(langford 4 #:symmetry-breaking? #f)
(newline)

(displayln "Langford n=4..8 with symmetry breaking:")
(for ([n (range 4 9)])
  (show "n" n)
  (let ([sols (time (langford n #:symmetry-breaking? #t))])
    (show "num-sols:" (length sols))
    (for ([sol sols])
      (displayln sol))
    (newline)
  ))
  
