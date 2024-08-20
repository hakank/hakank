#|
  Euler #24 in Racket

  Problem 24
  """
  A permutation is an ordered arrangement of objects. For example, 3124 is one 
  possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are 
  listed numerically or alphabetically, we call it lexicographic order. The 
  lexicographic permutations of 0, 1 and 2 are:
   
      012   021   102   120   201   210
  
  What is the millionth lexicographic permutation of the digits 
  0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in math/number-theory
                  factorial
                  ))
;;; (require racket/trace)

(require (only-in "utils_hakank.rkt"
                  time-function digits->number permutations-numbers-lex
                  vector->number vector-next-permutation list<=?
                  div
                  ))

;;;
;;; From https://rosettacode.org/wiki/Permutations#Racket
;;;
(define (lperms s)
  (cond [(empty? s) '()]
        [(empty? (cdr s)) (list s)]
        [else
         (let splice ([l '()][m (car s)][r (cdr s)])
           (append
            (map (lambda (x) (cons m x)) (lperms (append l r)))
            (if (empty? r) '()
                (splice (append l (list m)) (car r) (cdr r)))))]))

;;; From my Common Lisp program euler24.lisp
;;; (I'm not sure about the original source.)
;;; Ah, but it's not lexicographic!!!!
(define (permutations2 bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null? bag)
      '(())
      ;; Otherwise, take an element, e, out of the bag.
      ;; Generate all permutations of the remaining elements.
      ;; And add e to the front of each of these.
      ;; Do this for all possiblem e to generate all permutations.
      (append-map (lambda (e)
               (map (lambda (p) (cons e p))
                    (permutations (remove e bag) )))
              bag))
  )


;;; Way too slow!
;;; cpu time: 18286 real time: 18286 gc time: 2180
(define (euler24a)
  (list-ref (sort (map (lambda (p) (~a p) ) (permutations (range 0 10))) string<?) (sub1 1000000))

  )

;;; Faster but still too slow!
;;; cpu time: 3728 real time: 3728 gc time: 2807
(define (euler24b)
  (digits->number (list-ref (lperms (range 0 10)) (sub1 1000000)))
  )

;;; Nope, permutations2 is not lexicographic.
;; (define (euler24c)
;;   (list-ref (permutations2 (range 0 10)) (sub1 1000000))  
;;   )

;;; Nope, not with faster permutations-numbers-lex,
;;; probably too long pre-processing time...
;;; cpu time: 3988 real time: 3988 gc time: 1007
(define (euler24d)
  (list-ref (permutations-numbers-lex (range 0 10) <) (sub1 1000000))  
  )

;;; Better
;;; cpu time: 302 real time: 302 gc time: 36
(define (euler24e)
  (let ([p (list->vector(range 0 10))])
    (vector->number (for/last ([i (range (sub1 1000000))]
               #:do [(set! p (vector-next-permutation p))])
      p)
    ))
  )


;;; Using (sort ... list<=?)
;;; Nope: Too slow (as expected since there's a lot of lists to compare)
;;; cpu time: 10465 real time: 10473 gc time: 988
(define (euler24f)
  (list-ref (sort (permutations (range 0 10)) list<=?) (sub1 1000000))

  )

;;; This is port of my Picat program euler24.pi (function euler24/0)
;;; """inspired by a solution on the 'net"""
;;; (not sure about the source of this algorithm)
;;; Faster
;;; cpu time: 0 real time: 0 gc time: 0
(define (euler24g)
  (let* ([n 999999]
         [p 10]
         [eli (for/list ([i (range 1 (add1 p))]) (modulo i 10))]
         [answer '()])
         (for ([i (range 1 p)])
           (let* ([f (factorial (- p i))]
                  [d (div n f)]
                  [e (list-ref eli (sub1 d))])
             (set! n (modulo n f))
             (set! answer (append answer (list e)))
             (set! eli (remove e eli))
             )
           )
         (digits->number (append answer eli)))
  )


(define (run)
  ;;; (time-function euler24a)
  ;;; (time-function euler24b) 
  ;;; (time-function euler24c) ;;; Not correct
  ;;; (time-function euler24d)
  ;;; (time-function euler24e) 
  ;;; (time-function euler24f)
  (time-function euler24g)
  )

(run)
