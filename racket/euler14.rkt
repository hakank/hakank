#|
  Euler #14 in Racket

  Problem 14
  """
  The following iterative sequence is defined for the set of positive integers:

  n n/2 (n is even)
  n 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following 
  sequence:
  13 40 20 10 5 16 8 4 2 1

  It can be seen that this sequence (starting at 13 and finishing at 1) 
  contains 
  10 terms. Although it has not been proved yet (Collatz Problem), it is 
  thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.)
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require memoize)

(require (only-in "utils_hakank.rkt"
                  time-function
                  ))

;; Optimized Collatz function
(define (collatz n)
  (if (even? n)
      (quotient n 2)
      (+ (* n 3) 1)))

;; Main function to find the longest Collatz sequence
(define (longest-seq limit)
  (define lens (make-vector limit 0))
  (define max-len 0)
  (define max-n 1)
  
  (for ([n (in-range 3 limit 2)])
    (let ([m n]
          [c-len 1])
      (let loop ()
        (cond
          [(= m 1) (void)]
          [(and (< m limit) (> (vector-ref lens m) 0))
           (set! c-len (+ c-len (vector-ref lens m) -1))]
          [else
           (set! m (collatz m))
           (set! c-len (add1 c-len))
           (loop)]))
      
      (when (< n limit)
        (vector-set! lens n c-len))
      
      (when (> c-len max-len)
        (set! max-len c-len)
        (set! max-n n))))
  
  max-n)

;; Function to solve Euler problem 14
(define (euler14a)
  (longest-seq 1000001))

;; Run and measure the time for the solution
(define (run)
  (time-function euler14a))

(run)

