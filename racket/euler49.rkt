#|
  Euler #49 in Racket

  """
  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms 
  increases by 3330, is unusual in two ways: (i) each of the three terms are 
  prime, and, (ii) each of the 4-digit numbers are permutations of one another.

  There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
  exhibiting this property, but there is one other 4-digit increasing sequence.

  What 12-digit number do you form by concatenating the three terms 
  in this sequence?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in math/number-theory
                  prime?))

(require (only-in "utils_hakank.rkt"
                  time-function
                  ))

(define (char-list->string lst)
  (map (lambda (cs) (apply string cs)) lst)
  )

(define (number-list->string lst)
  (apply string-append (map (lambda (cs) (number->string cs)) lst))
  )

(define (string-permutations s)
  (let ([chars (string->list s)])
    ;;; (remove-duplicates (map (lambda (cs) (apply string cs)) (permutations chars)))
    (remove-duplicates (char-list->string (permutations chars)))
    )
  )

(define (get-element n perms diff)
  (let ([res 0])
    (for ([p perms])
      (let ([pp (string->number p)])
        (when (and (> pp n) (= (- pp n) diff))
          (set! res pp)))
      )
    res)
  )


(define (check-perms n diff)
  (let ([all-perms (string-permutations (number->string n))]
        [ll '()])
    (when (> (length all-perms) 0)
      (let ([p1 (get-element n all-perms diff)]
            [p2 0])
            (when (> p1 0)
              (set! p2 (get-element p1 all-perms diff)))
            (when (> p2 0)
              (set! ll (list n p1 p2)))
            )
      )
    ll)
  )

;;; Perhaps a little too messy...
;;; cpu time: 96 real time: 97 gc time: 31
(define (euler49a)
  (let ([diff 3330]
        [res 0])
    (for/last ([n (range 1001 10000 2)]
               #:do [(define cp (check-perms n diff))]
               #:when (and (not (= n 1487)) (prime? n) (> (length cp) 0)))
      ;;; (apply string-append (map (lambda (c) (number->string c)) cp))
      (number-list->string cp)
      )
    )
  )

(define (run) 
  (time-function euler49a)
  )

(run)
