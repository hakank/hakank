#|
  Euler #23 in Racket

  Problem 23
  """
  A perfect number is a number for which the sum of its proper divisors 
  is exactly equal to the number. For example, the sum of the proper divisors 
  of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

  A number n is called deficient if the sum of its proper divisors is less than 
  n and it is called abundant if this sum exceeds n.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number 
  that can be written as the sum of two abundant numbers is 24. By mathematical 
  analysis, it can be shown that all integers greater than 28123 can be written 
  as the sum of two abundant numbers. However, this upper limit cannot be reduced 
  any further by analysis even though it is known that the greatest number that 
  cannot be expressed as the sum of two abundant numbers is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum of 
  two abundant numbers.
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require "utils_hakank.rkt")

(define (get-abundant)
  (let* ([limit 20161]
         [arr (make-vector (add1 limit) 1)])
    (for* ([i (range 2 limit)]
           [j (range (* i 2) limit i)])
      (vector-set! arr j (+ (vector-ref arr j) i))
    )
    (list arr
          (for/list ([i (range 12 limit)]
                     #:when (> (vector-ref arr i) i))
            i)
          )
    )
  )

;;; This is faster than my Picat version (euler23.pi which takes about 0.507s). Neat!
;;; cpu time: 203 real time: 203 gc time: 0
(define (euler23a)
  (let* ([limit (add1 20161)]
         [t (get-abundant)]
         [arr (first t)]
         [abundant (second t)])
    (for* ([a abundant]
           [b abundant])
      (when (< (+ a b) limit)
        (vector-set! arr (+ a b) 0)
        )
      )
    
    (for/sum ([i (range 1 limit)]
              #:when (> (vector-ref arr i) 0))
      i)
    )
  )

(define (run)
  (time-function euler23a)
  )

(run)



