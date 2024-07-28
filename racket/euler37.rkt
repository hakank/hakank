#|
  Euler #37 in Racket

  """
  The number 3797 has an interesting property. Being prime itself, it is possible to 
  continuously remove digits from left to right, and remain prime at each stage: 
  3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

  Find the sum of the only eleven primes that are both truncatable from left to right 
  and right to left.

  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  """

  TODO: A little too messy (and a little too slow).

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require math/number-theory)
;;; (require racket/trace)

(require "utils_hakank.rkt")

(define (truncate-number ds)
  (let ([len (length ds)]
        [ds-rev (reverse ds)])
    (flatten (for/list ([i (range len)])
      (list (digits->number (slice ds i))
            (digits->number (reverse (slice ds-rev (- (sub1 len) i))))) 
      ))
    )
  )


(define (check37 n)
  (and (prime? n) (andmap prime? (truncate-number (number->digits n))))
  )

;;;
;;; Check for primes directly, slightly faster
;;;
(define (truncate-number-b ds)
  (let ([len (length ds)]
        [ds-rev (reverse ds)])
    (andmap prime? (flatten (for/list ([i (range len)])
      (list (digits->number (slice ds i))
            (digits->number (reverse (slice ds-rev (- (sub1 len) i))))) 
      )))
    )
  )


;;; A little messy with the checked variable to avoid two calls to checked.
;;; cpu time: 524 real time: 525 gc time: 89
(define (euler37a)
  (let ([c 0]
        [checked #f])
    (for/sum ([n (in-naturals 11)]
              #:break (= c 11)
              #:do [(if (check37 n)
                        (begin 
                          (set! c (add1 c))
                          (set! checked #t))
                        (set! checked #f))
                    ]
              #:when (equal? #t checked)
              )
      n)
    )
  )


(define (check37-b n)
  (and (prime? n) (truncate-number-b (number->digits n)))
  )


;;; Variant with an alternative truncate.
;;; A little faster.
;;; cpu time: 507 real time: 507 gc time: 87
(define (euler37b)
  (let ([c 0]
        [checked #f])
    (for/sum ([n (in-naturals 11)]
              #:break (= c 11)
              #:do [(if (check37-b n)
                        (begin 
                          (set! c (add1 c))
                          (set! checked #t))
                        (set! checked #f))
                    ]
              #:when (equal? #t checked)
              )
      n)
    )
  )


(define (run)
  ;;; (time-function euler37a)
  (time-function euler37b)
  )

(run)





