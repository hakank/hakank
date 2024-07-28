#|
  Euler #39 in Racket

  """
  If p is the perimeter of a right angle triangle with integral length sides, 
  {a,b,c}, there are exactly three solutions for p = 120.
   
  {20,48,52}, {24,45,51}, {30,40,50}
   
  For which value of p <= 1000, is the number of solutions maximised?
  """


  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require "utils_hakank.rkt")

;;; Port of my Picat program euler39.pi (euler39e)
;;; Again, quite messy
;;; cpu time: 208 real time: 208 gc time: 42
(define (euler39a)
  (let* ([n 500]
         [squares (for/hash ([i (range 1 n)]) (values (expt i 2) 1))]
         [freq (make-hash)]
         [valid (for*/list ([x (hash-keys squares)]
                            [y (hash-keys squares)]
                            #:do [(define c (round (+ (sqrt x) (sqrt y) (sqrt (+ x y)))))]
                            #:when (and (< x y) (hash-has-key? squares (+ x y)) (<= c 1000 )))
                  ;; Update frequency table
                  (if (hash-has-key? freq c)
                      (hash-set! freq c (add1 (hash-ref freq c)))
                      (hash-set! freq c 1))
                  c)]
         )
    (let ([max (apply max (hash-values freq))])      
      (for/first ([k (hash-keys freq)]
                 #:when (= (hash-ref freq k) max))
        k)
      )
    )
    
  )

(define (run)
  (time-function euler39a)
  )

(run)
