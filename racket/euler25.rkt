#|
  Euler #25 in Racket

  Problem 25
  """
  The Fibonacci sequence is defined by the recurrence relation:

     Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
  
  Hence the first 12 terms will be:

     F1 = 1
     F2 = 1
     F3 = 2
     F4 = 3
     F5 = 5
     F6 = 8
     F7 = 13
     F8 = 21
     F9 = 34
     F10 = 55
     F11 = 89
     F12 = 144

  The 12th term, F12, is the first term to contain three digits.

  What is the first term in the Fibonacci sequence to contain 1000 digits?")
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in math/number-theory
                  fibonacci
                  ))
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function
                  ))

;;; cpu time: 159 real time: 159 gc time: 24
(define (euler25a)
  (for/first ([i (in-naturals 1)]
              #:when (= (string-length (number->string (fibonacci i))) 1000))
    i
    )
  )

(define (run)
  (time-function euler25a)
  )

(run)



