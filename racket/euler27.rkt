#|
  Euler #27 in Racket

  Problem 27
  """
  Euler published the remarkable quadratic formula:

  n^2 + n + 41

  It turns out that the formula will produce 40 primes for the consecutive values 
  n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 
  41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

  Using computers, the incredible formula  n^2 − 79n + 1601 was discovered, which 
  produces 80 primes for the consecutive values n = 0 to 79. The product of the 
  coefficients, −79 and 1601, is −126479.

  Considering quadratics of the form:

      n^2 + an + b, where |a| < 1000 and |b| < 1000

      where |n| is the modulus/absolute value of n
      e.g. |11| = 11 and |−4| = 4

  Find the product of the coefficients, a and b, for the quadratic 
  expression that produces the maximum number of primes for consecutive 
  values of n, starting with n = 0.
  """

  TODO: Too messy, too slow

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require math/number-theory)
;;; (require racket/trace)

(require "utils_hakank.rkt")

;;; This works but it's way too messy and too slow
;;; cpu time: 1301 real time: 1302 gc time: 911
(define (euler27a)
  (let* ([t 999]
         [ans (first
               (sort (filter (lambda (ll) (> (first ll) 1) )
                             (for*/list ([a (range (- t) t)]
                                         [b (range (- t) t)])
                               (list (for/sum ([len (in-naturals 1)]
                                               #:do [(define pp (+ (expt len 2) (* a len) b))]
                                               #:break (not (and (> pp 0) (prime? pp))))
                                       1
                                       ) a b)
                               )) > #:key first))])
    (* (second ans) (third ans))
    
    )
  )

;;; This is a port of my Picat euler27/0 function (euler27.pi)
;;; Faster (though arguably messier)
;;; cpu time: 207 real time: 207 gc time: 79
(define (euler27b)
  (let ([t 999]
        [best-len 0]
        [best-a 0]
        [best-b 0])
    (for* ([a (range (- t) (add1 t))]
           [b (range (- t) (add1 t))])
      (let* ([len 0]
             [pp (+ (expt len 2) (* a len)  b)])
        (for ([tmp (in-naturals)]
              #:break (nor (and (> pp 1) (prime? pp))))
          (set! len (add1 len))
          (set! pp (+ (expt len 2) (* a len)  b))
          (when (> len best-len)
            (set! best-len len)
            (set! best-a a)
            (set! best-b b)
            ))))
    (* best-a best-b))
  )

(define (run)
  ;;; (time-function euler27a)
  (time-function euler27b) 
  )

(run)




