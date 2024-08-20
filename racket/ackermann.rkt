#| 

  Ackermann function in Racket.

  From Rosetta code:
  http://rosettacode.org/wiki/Ackermann_function
  """
  The Ackermann function is a classic recursive example in computer science. 
  It is a function that grows very quickly (in its value and in the size of 
  its call tree). It is defined as follows:

      A(m, n) = n+1 if m = 0
                A(m-1, 1) if m > 0 and n = 0
                A(m-1, A(m, n-1)) if m > 0 and n > 0

  Its arguments are never negative and it always terminates. Write a function 
  which returns the value of A(m,n). Arbitrary precision is preferred (since 
  the function grows so quickly), but not required. 
  """

  Note: The un-memoized ackermann2/2
     expt: out of memory 
  for ackermann (4 3).
  As does my cached version ackermann3/2
  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))
;;; (require (except-in math/number-theory permutations))

(require memoize)
;;; (require racket/trace)
(require "utils_hakank.rkt")

; Memoized version
(define/memo (ackermann m n)
  (cond [(= m 0) (add1 n)]
        [(and (> m 0) (= n 0)) (ackermann (sub1 m) 1)]
        [(and (> m 0) (> n 0)) (ackermann (sub1 m) (ackermann m (sub1 n)))]
        )
  )

;; Based on the Python solution
;; Faster than ackerman/2
;; (note: it's not memoized)
(define (ackermann2 m n)
  ;;; (displayln (format "ackermann ~a ~a" m n))
  (match (list m n)
    [(list 0 n) (+ n 1)]
    [(list 1 n) (+ n 2)]
    [(list 2 n) (+ (* n 2) 3)]
    [(list 3 n) (+ (* 8 (sub1 (expt 2 n))) 5)]
    [(list m n) (if (= n 0)
              (ackermann2 (sub1 m) 1)
              (ackermann2 (sub1 m) (ackermann2 m (sub1 n))))]
        )
  )

;;;
;;; Using a 'manual' hash.
;;; Note: This is not faster than the un-memoized ackermann2
;;; 
(define (ackermann3 m n)
  (define (ackermann3-hash h m n)
    (if (hash-has-key? h (list m n))
        (hash-ref h (list m n))
        (let ([val (match (list m n)
                     [(list 0 n) (+ n 1)]
                     [(list 1 n) (+ n 2)]
                     [(list 2 n) (+ (* n 2) 3)]
                     [(list 3 n) (+ (* 8 (sub1 (expt 2 n))) 5)]
                     [(list m n) (if (= n 0)
                                     (ackermann3-hash h (sub1 m) 1)
                                     (ackermann3-hash h (sub1 m) (ackermann3-hash h m (sub1 n))))]
                     )])
          (hash-set! h (list m n) val)
          val
          )
        )
    )
  (ackermann3-hash (make-hash) m n)
  )



(define (test)
 
  ; (trace ackermann)
  (time (writeln (list "ackermann(3 4)" (ackermann 3 4))))
  ; (untrace ackermann)
  (newline)
  
  (time (writeln (list "ackermann(3 14)" (ackermann 3 14))))
  (newline)
  
  (time (writeln (list "ackermann2(3 4)" (ackermann2 3 4))))
  (newline)

  
  (writeln (list "ackermann2(3 14)" (ackermann2 3 14)))

  (for/list ([n (range 11)])
    (newline)
    (time (displayln (list "ackerman 3" n (ackermann 3 n))))
    (time (displayln (list "ackerman2 3" n (ackermann2 3 n))))
    (flush-output)
    )

  (for/list ([n (range 5)])
    (newline)
    (let* ([res (ackermann2 4 n)]
           ;;; [res (ackermann3 4 n)]
           [str (number->string res)]
           [str-len (string-length str)])
      (displayln (list "str-len:" str-len))
      (if (> str-len 100)
          (time (displayln (list "ackerman2 4 " n (list
                                                   (substring str 0 10)
                                                   "..."
                                                   (substring str (- str-len 10) str-len))
                                 )))
          (displayln (list "ackerman2 4 " n res)))
      
      (flush-output)
      )
    )

  #t
  )

(test)
