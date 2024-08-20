#| 

  Inducing arithmetic functions in Racket Gamble.

  From http://forestdb.org/models/arithmetic.html (Church model)

  This is part I. 
  See gamble_inducing_arithmetic_functions2.rkt for part II.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)

(require "gamble_utils.rkt")

(define (random-arithmetic-fn)
  (if (flip 0.3)
      (random-combination (random-arithmetic-fn) 
                          (random-arithmetic-fn))
      (if (flip) 
          (lambda (x) x) 
          (random-constant-fn))))

(define (random-combination f g)
  (define op (uniform-draw (list + -)))
  (lambda (x) (op (f x) (g x))))

(define (random-constant-fn)
  ; (define i (sample-integer 10))
  (define i (discrete-uniform 10))
  (lambda (x) i))

(define (sample)
  (rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define my-proc (random-arithmetic-fn))
   ;;; (displayln (list "my-proc 2" (my-proc 2)))
   (observe/fail (and (= (my-proc 0) 2)
                      (= (my-proc 1) 3)))
   (my-proc 2))
  )

; -> (discrete-dist [4 1.0]
(sampler->discrete-dist (sample) 1000)
; -> 4
(sampler->mean (sample) 1000)

