#| 

  Inducing Arithmetic Functions (Church) in Racket/Gamble 

  This is slightly adapted Church models from 
  http://forestdb.org/models/arithmetic.html
  """
  [Part 1:]
  This program induces a deterministic arithmetic function from input-output examples.

  ...

  [Part 2]
  To induce stochastic functions, it can be useful to condition on the evidence -
  our input-output examples – having high marginal likelihood under the induced function:

  ...
   
  This is semantically equivalent to conditioning on input-output examples directly, but can 
  improve convergence rates when MCMC is used in the outer query.

  References:

  Probabilistic Models of Cognition. Noah D. Goodman and Joshua B. Tenenbaum (2011).
  [https://probmods.org/]
  """

  Output for the second part:

(4 : 0.5837414299706171)
(8 : 0.058765915768854066)
(6 : 0.04407443682664055)
(7 : 0.04407443682664055)
(5 : 0.043095004897159644)
(2 : 0.04113614103819784)
(9 : 0.04113614103819784)
(0 : 0.040156709108716944)
(1 : 0.03819784524975514)
(3 : 0.03623898139079334)
(10 : 0.0039177277179236044)
(11 : 0.0039177277179236044)
(-1 : 0.002938295788442703)
(-2 : 0.002938295788442703)
(-4 : 0.002938295788442703)
(-5 : 0.002938295788442703)
(12 : 0.0019588638589618022)
(13 : 0.0019588638589618022)
(-3 : 0.0019588638589618022)
(-6 : 0.0019588638589618022)
(-7 : 0.0019588638589618022)
(mean: 4.258570029382958)

-7:   1 # (0.001 / 0    )
-6:   1 # (0.001 / 0.001)
-5:   2 # (0.002 / 0.002)
-4:   2 # (0.002 / 0.004)
-3:   1 # (0.001 / 0.006)
-2:   2 # (0.002 / 0.007)
-1:   2 # (0.002 / 0.009)
 0:  40 ###### (0.04  / 0.011)
 1:  38 ###### (0.038 / 0.051)
 2:  41 ###### (0.041 / 0.089)
 3:  36 ##### (0.036 / 0.13 )
 4: 595 ################################################################################ (0.595 / 0.166)
 5:  43 ###### (0.043 / 0.761)
 6:  44 ###### (0.044 / 0.804)
 7:  44 ###### (0.044 / 0.848)
 8:  59 ######## (0.059 / 0.892)
 9:  41 ###### (0.041 / 0.951)
10:   3 # (0.003 / 0.992)
11:   3 # (0.003 / 0.995)
12:   1 # (0.001 / 0.998)
13:   1 # (0.001 / 0.999)



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


;; "This program induces a deterministic arithmetic function from input-output examples."
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
  (define i (sample-integer 10))
  (lambda (x) i))

(define (sample)
  (rejection-query
   (define my-proc (random-arithmetic-fn))
   (my-proc 2)
   #:when
   (and (= (my-proc 0) 2)
        (= (my-proc 1) 3))))

; outputs 4
(sample)


;; """
;; To induce stochastic functions, it can be useful to condition on the evidence -
;; our input-output examples – having high marginal likelihood under the induced function:
;; ""
(define (random-arithmetic-fn2)
  (if (flip 0.3)
      (random-combination2 (random-arithmetic-fn2) 
                          (random-arithmetic-fn2))
      (if (flip) 
          (lambda (x) x) 
          (random-constant-fn))))

(define (random-combination2 f g)
  (if (flip 0.5)
    ((lambda (op) (lambda (x) (op (f x) (g x))))
      (uniform-draw (list + -)))
    (lambda (x) (sample-integer 10))))

(define (random-constant-fn2)
  (define i (sample-integer 10))
  (lambda (x) i))

(define (find-prob x xs ps)
  (if (null? xs)
      0.0
      (if (equal? (first xs) x)
          (first ps)
          (find-prob x (rest xs) (rest ps)))))

(define (likelihood fn x)
  (let ([dist (enumeration-query (define _ 1) (fn) #:when #t)])
    (find-prob x (first dist) (second dist))))

(define (sample2)
  (rejection-query
   (define my-proc (random-arithmetic-fn2))
   (define (my-proc-likelihood x y)
     (likelihood (lambda () (my-proc x)) y))
   (my-proc 2)
   #:when
   (and (flip (my-proc-likelihood 0 2)) 
        (flip (my-proc-likelihood 1 3)))))

;; (hist (repeat 100 sample2))
(define the-sample (repeat (lambda () (sample2)) 1000))
(show-freq the-sample)
(newline)
(show-histogram the-sample)
