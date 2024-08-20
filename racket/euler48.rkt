#|
  Euler #48 in Racket

  """
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
  
  Find the last ten digits of the series, 
  1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require (except-in math/number-theory permutations))
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function string-last
                  ))


;;; cpu time: 7 real time: 7 gc time: 0
(define (euler48a)
  (let* ([t 10000000000]
         [s (number->string (for/sum ([i (range 1 1001)])
                              (let ([n i])
                                (for ([j (range 2 (add1 i))])
                                  (set! n (modulo (* n i) t)))
                                n)
                              )
                            )])
    (string-last s 10)
    )
  )

;;; Neater but slower
;;; cpu time: 20 real time: 20 gc time: 9
(define (euler48b)
  (let ([s 1])
    (for ([i (range 2 1001)])
      (set! s (+ s (expt i i))))
    (string-last (number->string s) 10))
  )

(define (run) 
  (time-function euler48a)
  ;;; (time-function euler48b)
  )

(run)
