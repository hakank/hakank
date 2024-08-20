#| 

  Inducing arithmetic functions 2 in Racket Gamble.

  From http://forestdb.org/models/arithmetic.html (Church model)

  This is part 2. 

  This port required some trickery, especially in the likelihood function.

  Output:

  (discrete-dist [-6 0.0009999999999999998] [-5 0.0019999999999999996] [-4 0.002999999999999999] [-3 0.002999999999999999] [-2 0.002999999999999999] [-1 0.003999999999999999] [0 0.03899999999999999] [1 0.04599999999999999] [2 0.04499999999999999] [3 0.05699999999999999] [4 0.5769999999999998] [5 0.031999999999999994] [6 0.04299999999999999] [7 0.046999999999999986] [8 0.04899999999999999] [9 0.040999999999999995] [10 0.003999999999999999] [11 0.003999999999999999])

  Mean: 4.163

  Using (show-freq 1000) in about 8s)
(4 : 0.5752212389380531)
(3 : 0.06096361848574238)
(5 : 0.05408062930186824)
(9 : 0.0471976401179941)
(1 : 0.04424778761061947)
(6 : 0.04424778761061947)
(7 : 0.04129793510324484)
(0 : 0.03736479842674533)
(2 : 0.0344149459193707)
(8 : 0.03146509341199607)
(-1 : 0.00688298918387414)
(10 : 0.0058997050147492625)
(-2 : 0.004916420845624385)
(11 : 0.003933136676499509)
(-4 : 0.003933136676499509)
(13 : 0.0019665683382497543)
(-3 : 0.0019665683382497543)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)

(require "gamble_utils.rkt")

;;; """
;;; To induce stochastic functions, it can be useful to condition on the evidence
;;; - our input-output examples - having high marginal likelihood under the induced
;;; function:
;;; """

(define (random-arithmetic-fn)
  (if (flip 0.3)
      (random-combination (random-arithmetic-fn) 
                          (random-arithmetic-fn))
      (if (flip) 
          (lambda (x) x) 
          (random-constant-fn))))

(define (random-combination f g)
  (if (flip 0.5)
    ((lambda (op) (lambda (x) (op (f x) (g x))))
      (uniform-draw (list + -)))
    (lambda (x) (discrete-uniform 10))))

(define (random-constant-fn)
  ; (define i (sample-integer 10))
  (define i (discrete-uniform 10))
  (lambda (x) i))

(define (find-prob x xs ps)
  (if (null? xs)
      0.0
      (if (equal? (first xs) x)
          (first ps)
          (find-prob x (rest xs) (rest ps)))))

(define (likelihood fn x)
  ;;; hakank: Some trickery needed here
  (let* ([dist1 (enumerate
                 (define _ 1) #t (fn))]
         [vals (vector->list (discrete-dist-values dist1))]
         [weights (vector->list (discrete-dist-weights dist1))]         
         )
    ; (find-prob x (first dist) (second dist))))
    (find-prob x vals weights)))

(define (sample)
  (rejection-sampler
   ; importance-sampler ; error
   ; mh-sampler ; errors/warnings proposal produced impossible value dist: (bernoulli-dist 1)  value: 0

   
   (define my-proc (random-arithmetic-fn))
   ;; (displayln (list "my-proc" (my-proc 2)))
   (define (my-proc-likelihood x y)
     (likelihood (lambda () (my-proc x)) y))
   (observe/fail (and (flip (my-proc-likelihood 0 2)) 
                      (flip (my-proc-likelihood 1 3))))
   
   (my-proc 2))
  )

;;; """
;;; This is semantically equivalent to conditioning on input-output examples
;;; directly, but can improve convergence rates when MCMC is used in the outer query.
;;; """

(show-freq (repeat (sample) 1000))
;;;(show-model (sample))

