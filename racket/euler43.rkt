#|
  Euler #43 in Racket

  """
  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of 
  each of the digits 0 to 9 in some order, but it also has a rather interesting 
  sub-string divisibility property.
  
  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we 
  note the following:
  
      * d2d3d4=406 is divisible by 2
      * d3d4d5=063 is divisible by 3
      * d4d5d6=635 is divisible by 5
      * d5d6d7=357 is divisible by 7
      * d6d7d8=572 is divisible by 11
      * d7d8d9=728 is divisible by 13
      * d8d9d10=289 is divisible by 17
  
  Find the sum of all 0 to 9 pandigital numbers with this property.
  ""

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in math/number-theory
                  factorial))

(require (only-in "utils_hakank.rkt"
                  time-function number->digits chunks-of digits->number
                  vector->number vector-next-permutation 
                  ))

(define (check43a n)
  (let ([ps '(2 3 5 7 11 13 17)])
    (let ([nums (number->digits n)])
    (for/and ([p ps]
              [c (chunks-of (drop nums 1) 3)])
      (= (modulo (digits->number c) p) 0)
      )
    ))
  )

;;; Way too slow!
;;; cpu time: 7826 real time: 7836 gc time: 115
(define (euler43a)
   (for/sum ([p (in-permutations (range 0 10))]
               #:do [(define n (digits->number p))]
               #:when (and (not (= (first p) 0) ) (check43a n) (writeln p))
               )
    n)
  )

;;; Using vector-permutation
;;; Slower 
;;; cpu time: 8847 real time: 8855 gc time: 107
(define (euler43b)
  (let* ([n 10]
         [lst (range n)]
         ;;; [lst '(1 2 3 4 5 6 7 8 9 0)] % slower than (range n)
         [f (factorial n)]
         [p-orig (list->vector lst)]
         [p-prev p-orig]
         [p p-orig]
         [rev-p (list->vector (reverse (vector->list p-orig)))])
    (for/sum ([i (range f)]
              ;;; #:break (= (vector-ref p 0) 0)              
              #:break (equal? p rev-p)
              #:do [(set! p-prev p)
                    (define num (vector->number p-prev))
                    (set! p (vector-next-permutation p))
                    ;;; (writeln p)
                    ]
              ;;; #:when (and (> (vector-ref p 0) 0) (check43a num)) ; slightly slower?
              #:when (and (not (= (vector-ref p 0) 0)) (check43a num))              
              )
      num)
   )
  
  )

;;; Skipping the chunks-of from check43a
;;; But still too slow!
(define (check43c n)
  (let* ([ps '(2 3 5 7 11 13 17)]
         [nums (number->digits n)]
         [len (length ps)])
    (for/and ([p ps]
              [c (range 1 (add1 len)) ])
      (let ([c-list (for/list ([cc (range 3)])
                      (list-ref nums (+ c cc)))])
        (= (modulo (digits->number c-list)
                 p) 0
           ) ))
    ))

(define (check43c-2 n)
  (let* ([ps '(2 3 5 7 11 13 17)]
         [nums (number->digits n)]
         [len (length ps)])
    (for/and ([p ps]
              [c (range 1 (add1 len)) ])
      (= (modulo (digits->number (for/list ([cc (range 3)])
                      (list-ref nums (+ c cc))))
                 p) 0)
      )
    )
    )

;;; Faster by replacing (digits->number ...) to (+ ...)
(define (check43c-3 n)
  (let* ([ps '(2 3 5 7 11 13 17)]
         [nums (number->digits n)]
         [len (length ps)])
    (for/and ([p ps]
              [c (range 1 (add1 len)) ])
      (= (modulo (+ (* 100 (list-ref nums (+ c 0 )))
                    (* 10 (list-ref nums (+ c 1 )))
                    (list-ref nums (+ c 2 )))
                 p) 0)
      )
    )
  )

;;; cpu time: 3760 real time: 3760 gc time: 98
(define (euler43c)
   (for/sum ([p (in-permutations (range 0 10))]
               #:do [(define n (digits->number p))]
               #:when (and (> (first p) 0) (check43c-3 n))
               )
    n)
  )

;;; Reversing the order of primes and the indices,
;;; perhaps a little faster than euler43c-3.
(define (check43d-1 n)
  ;;; (let* ([ps (reverse '(2 3 5 7 11 13 17))]
  (let* ([ps '(17 13 11 7 5 3 2)]
         [nums (number->digits n)]
         [len (length ps)])
    (for/and ([p ps]
              [c (range len 0 -1)])
      (check-perm nums (+ c 0) (+ c 1) (+ c 2) p)
      )
    )
  )


(define (check-perm1 p a b c m)
  (zero? (modulo (+ (* 100 (vector-ref p a))
                    (* 10 (vector-ref p b))
                    (vector-ref p c))
                 m)))

(define (check-perm perm)
  (let* ([ps '(17 13 11 7 5 3 2)]
         [len (length ps)])
    (for/and ([p ps]
              [c (range len 0 -1)])
      (check-perm1 perm (+ c 0) (+ c 1) (+ c 2) p)
      )
    )
  
  )

;;; Faster than euler43c, but less neat.
;;; Ah, skipping converting to number until it's a valid permutation
;;; is much faster.
;;; cpu time: 398 real time: 398 gc time: 30
(define (euler43d)
  (define result 0)
  (define p (make-vector 10))
  
  (define (generate-permutations start)
    (if (= start 10)
        (when (and (> (vector-ref p 0) 0)
                   (check-perm1 p 7 8 9 17)
                   (check-perm1 p 6 7 8 13)
                   (check-perm1 p 5 6 7 11)
                   (check-perm1 p 4 5 6 7)
                   (check-perm1 p 3 4 5 5)
                   (check-perm1 p 2 3 4 3)
                   (check-perm1 p 1 2 3 2))
          (set! result (+ result (vector->number p))))
        (for ([i (in-range start 10)])
          (vector-swap! p start i)
          (generate-permutations (add1 start))
          (vector-swap! p start i))))
  
  (vector-copy! p 0 (vector 0 1 2 3 4 5 6 7 8 9))
  (generate-permutations 0)
  result)

;; Helper function to swap vector elements
(define (vector-swap! v i j)
  (let ([temp (vector-ref v i)])
    (vector-set! v i (vector-ref v j))
    (vector-set! v j temp)))

;; Helper function to convert vector to number
(define (vector->number v)
  (for/fold ([num 0])
            ([digit (in-vector v)])
    (+ (* num 10) digit)))

(define (run)
  ;;; (time-function euler43a)
  ;;; (time-function euler43b)
  ;;; (time-function euler43c)
  (time-function euler43d)

  )

(run)
