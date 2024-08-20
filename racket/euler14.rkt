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

(define (collatz n)
  (if (= (modulo n 2) 0)
      (/ n 2)
      (+ (* n 3) 1))
  )

;;; Using memoize
;;; Much slower than "plain" collatz
;;; cpu time: 10710 real time: 10727 gc time: 2331
;; (define/memo (collatz n)
;;   (if (= (modulo n 2) 0)
;;       (/ n 2)
;;       (+ (* n 3) 1))
;;   )

;; Slower
(define (collatz-hash n h)
  (if (hash-has-key? h n)
      (hash-ref h n)
      (let ([c (collatz n)])
        (hash-set! h n c)
        c)
      )
  )

(define (collatz-list n)
  (define (tmp n aux)
    (cond [(= n 1) (cons 1 aux)]
          [else (tmp (collatz n) (cons n aux))]
          )
    )
  ;; (reverse (tmp n '()))
  (tmp n '()) ; Skipping reverse
  )

(define (collatz-list-hash n h)
  (define (tmp n aux h)
    (cond [(= n 1) (cons 1 aux)]
          [else (tmp (collatz-hash n h) (cons n aux) h)]
          )
    )
  (tmp n '() h)
  )


;;; Too slow
;;; cpu time: 5101 real time: 5101 gc time: 282
;;; Skipping reverse in collatz-list
;;; cpu time: 4436 real time: 4443 gc time: 267
;;; Testing only odd numbers:
;;; cpu time: 2427 real time: 2431 gc time: 231
(define (euler14a)
  (second (first (sort (for/list ([n (range 3 1000001 2)])
                 (list (length (collatz-list n)) n)
                 ) > #:key first)))
  )

;;; Much slower:
;;; cpu time: 25181 real time: 25182 gc time: 338
(define (euler14b)
  (second (first (sort (for/list ([n (range 3 1000001 2)])
                 (list (length (collatz-list-hash n (make-hash))) n)
                 ) > #:key first)))
  )

;;;
;;; This is a port of my longest_seq/1 function from the Picat program euler14.pi.
;;; This concentrates only on the lengths of the sequences and uses a cache (lens).
;;; 
;;; Faster:
;;; cpu time: 976 real time: 976 gc time: 105
(define (longest-seq limit)
  (let ([lens (make-hash)]
        [max-len 0]
        [max-n 1]
        )
    ;;; Only checking the odd n's
    (for ([n (range 3 (add1 limit) 2)])
      (let ([m n]
            [c-len 1])
        (for ([t (in-naturals)]
              #:break (= m 1)) ; we've reached the target
          
          ;;; Is m in cache?
          (cond
            [(hash-has-key? lens m) (set! c-len (sub1 (+ c-len (hash-ref lens m))))
                                    (set! m 1)]
            [else  (set! m (collatz m))
                   (set! c-len (add1 c-len))]
            ))
        
        ;;; Update for n
        (hash-set! lens n c-len)
        
        ;;; Check/Update for max len
        (when (> c-len max-len)
          (set! max-len c-len)
          (set! max-n n))))
    max-n)
  )

(define (euler14c)
  (longest-seq 1000001)
  )

(define (run)
  ;;; (time-function euler14a)
  ;;;(time-function euler14b)
  (time-function euler14c)
  )

(run)

