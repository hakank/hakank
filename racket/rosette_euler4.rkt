#| 

  Euler problem #4 in Racket/Rosette.

  Problem 4
  """
  A palindromic number reads the same both ways. The largest palindrome made 
  from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  """

  The answer is 906609.

  Here are two approaches

  * euler4-1, "plain" assume and get-max-solution
    Time: 0.577s

    Solution: 906609
    '((9 0 6 913 993 906609 906609))

  * euler4-2, using solver-maximize
    Slower: about 1.8s

    (model
    [a 9]
    [b 0]
    [c 6]
    [d 913]
    [e 993]
    [p 906609])



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")
(require rosette/solver/smt/z3)

(current-solver (z3 #:logic 'QF_FD))

#|
  ;;;; ; cpu time: 16 real time: 9687 gc time: 0
  With QF_FD it's much faster
  cpu time: 17 real time: 577 gc time: 0
  (4 5 9 506 909 459954 459954)
  (4 7 0 506 929 470074 470074)
  (5 1 0 605 843 510015 510015)
  (8 2 8 858 966 828828 828828)
  (8 5 5 894 957 855558 855558)
  (9 0 6 913 993 906609 906609)
  (sol ((9 0 6 913 993 906609 906609)))

  906609

|#
(define (euler4-1)
  (clear-vc!)
  
  (define-symbolic a b c d e p opt integer?)
  (define vars (list a b c d e p opt))

  ; Note: For QF_FD to work, _all_ variables must
  ;       have a finte domain, hence the name QF_F(inite)D(omain).
  ; domains
  (assume (<= 1 a 9))
  (assume (<= 0 b 9))
  (assume (<= 0 c 9))
  (assume (<= 0 p (expt 10 6)))
  (assume (<= 0 opt (expt 10 6)))

  ; constraints
  (assume (= p (+ (* a 100001) (* b 10010) (* c 1100))))
  (assume (<= 100 d 999))
  (assume (<= 100 e 999))
  (assume (>= e d))
  (assume (= p (* d e)))
  (assume (= opt p))
  
  (define sol (get-max-solution vars opt #:debug? #t))
  (show "sol" sol)
  (last (first sol))

  )

; Using solver-maximize: Slower than with (euler4-2 with QF_FD)
; cpu time: 5 real time: 1226 gc time: 0
(define (euler4-2)
  (clear-vc!)
  
  (define solver (z3))  
  
  (define-symbolic a b c d e p integer?)
  (define vars (list a b c d e p))
  
  (define constraints  (list
                        ; domains
                        (<= 1 a 9)
                        (<= 0 b 9)
                        (<= 0 c 9)
                        (<= 0 p (expt 10 6))
                        ; constraints
                        (= p (+ (* a 100001) (* b 10010) (* c 1100)))
                        (<= 100 d 999)
                        (<= 100 e 999)
                        (>= e d)
                        (= p (* d e))))
    
    (solver-assert solver constraints)
    (solver-maximize solver (list p))
    (solver-check solver)

  )

(displayln "euler4-1")
(time (euler4-1))

(displayln "\neuler4-2")
(time (euler4-2))
