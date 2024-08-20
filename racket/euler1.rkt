#|
  Euler #1 in Racket

  Problem 1
  """
  If we list all the natural numbers below 10 that are multiples of 3 or 5, 
  we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;; #lang racket/base

(require (only-in math
                  sum))
(require (only-in "utils_hakank.rkt"
                  time-function
                  ))

;;; 0s total time 1.03s
(define (euler1a)
  (sum (filter (lambda (n) (or (= (modulo n 3) 0) (= (modulo n 5) 0))) (range 1 1000))))

;;; Recursion with accumulator
(define (e1b n max acc)
  (cond [(> n max) acc]
        [(or (= (modulo n 3) 0) (= (modulo n 5) 0))
         (e1b (+ 1 n) max (+ n acc))]
        [else (e1b (+ 1 n) max acc)]
        ))
(define (euler1b)
  (e1b 1 999 0))

;;; Using for/sum
(define (euler1c)
  (for/sum ([n (range 1 1000)]
             #:when (or (= (modulo n 3) 0) (= (modulo n 5) 0))
             )
    n))

;;; Using do
(define (euler1d)
  (let [(s 0)]
    (do ([i 1 (add1 i)])
        ((> i 999) s)
      (when (or (= (modulo i 3) 0) (= (modulo i 5) 0))
        (set! s (+ s i))))))

;; As euler1a but with a named function
(define (mod-3-or-5 n)
  (or (= (modulo n 3) 0) (= (modulo n 5) 0)))
(define (euler1e)
  (sum (filter mod-3-or-5 (range 1 1000))))

(define (run)
  (time-function euler1a)
  ;; (time-function euler1b)
  ;; (time-function euler1c)
  ;; (time-function euler1d)
  ;; (time-function euler1e)
  )

(define (run2)
  (euler1a)
  ;; (time-function euler1b)
  ;; (time-function euler1c)
  ;; (time-function euler1d)
  ;; (time-function euler1e)
  )

(run)

