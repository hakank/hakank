#| 

  Pill puzzle in Racket/Gamble 

  https://en.wikipedia.org/wiki/Pill_puzzle
  """
  The pill jar puzzle is a probability puzzle, which asks the expected value of the number of 
  half-pills remaining when the last whole pill is popped from a jar initially containing n 
  whole pills and the way to proceed is by removing a pill from the bottle at random. 
  If the pill removed is a whole pill, it is broken into two half pills. One half pill is 
  consumed and the other one is returned to the jar. If the pill removed is a half pill, 
  then it is simply consumed and nothing is returned to the jar.
  ...

  The expected value is then given by, E(X1) + E(X2) + ... + E(Xn). 
  Since E(Xk) = P(Xk = 1) = 1/(n − k + 1), the sought expected value is 
  1/n + 1/(n − 1) + 1/(n − 2) + ... + 1 = Hn (the nth harmonic number).
  """

  h is here the nth harmonic number.

  variable : half
  2: 0.24007
  1: 0.23961
  3: 0.19654
  4: 0.1446
  5: 0.09309
  6: 0.05087
  7: 0.02374
  8: 0.00894
  9: 0.00219
  10: 0.00035
  mean: 2.9193499999999992

  variable : h
  7381/2520: 1.0
  mean: 2.9289682539682538

  * Enumerate can be used for small values

  n: 1
  variable : half
  1: 1 (1.0)
  mean: 1 (1.0)

  variable : h
  1: 1 (1.0)
  mean: 1 (1.0)

  n: 2
  variable : half
  1: 1/2 (0.5)
  2: 1/2 (0.5)
  mean: 3/2 (1.5)

  variable : h
  3/2: 1 (1.0)
  mean: 3/2 (1.5)

  n: 3
  variable : half
  1: 7/18 (0.3888888888888889)
  2: 7/18 (0.3888888888888889)
  3: 2/9 (0.2222222222222222)
  mean: 11/6 (1.8333333333333333)

  variable : h
  11/6: 1 (1.0)
  mean: 11/6 (1.8333333333333333)

  n: 4
  variable : half
  1: 97/288 (0.3368055555555556)
  2: 97/288 (0.3368055555555556)
  3: 67/288 (0.2326388888888889)
  4: 3/32 (0.09375)
  mean: 25/12 (2.0833333333333335)

  variable : h
  25/12: 1 (1.0)
  mean: 25/12 (2.0833333333333335)

  n: 5
  variable : half
  1: 54997/180000 (0.3055388888888889)
  2: 54997/180000 (0.3055388888888889)
  3: 40927/180000 (0.22737222222222223)
  4: 2463/20000 (0.12315)
  5: 24/625 (0.0384)
  mean: 137/60 (2.283333333333333)

  variable : h
  137/60: 1 (1.0)
  mean: 137/60 (2.283333333333333)

  n: 6
  variable : half
  1: 460463/1620000 (0.28423641975308644)
  2: 460463/1620000 (0.28423641975308644)
  3: 356933/1620000 (0.220329012345679)
  4: 218893/1620000 (0.13511913580246915)
  5: 12281/202500 (0.06064691358024692)
  6: 5/324 (0.015432098765432098)
  mean: 49/20 (2.45)

  variable : h
  49/20: 1 (1.0)
  mean: 49/20 (2.45)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

#|
  Harmonic number 
  https://en.wikipedia.org/wiki/Zipf%27s_law
  H,N
|#
(define (harmonic_number n)
  (sum (for/list ([i n]) (/ 1 (add1 i)))))

#|
  Generatlized Harmonic number
  Hs,N
|#
(define (harmonic_number_generalized n s) 
  (sum (for/list ([i n]) (/ 1 (expt (add1 i) s) ) n)))

(define (model [n 5])
  (show "n" n)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define n 10) ; Initial number of (whole) pills

   (define (f a) 
     (define w (count-occurrences-eq "w" a))
     (if (= w 0)
         ; How many half pills are left?
         (count-occurrences-eq "h" a)
         ; Pick a pill at random
         (let* ((len (length a))
               (pick (random-integer len))
               (val (list-ref a pick))
               ; Remove that pill (I don't know a pure functional way to do this).
               ; This is quite slow:
               (v (range len))
               (filtered (filter (lambda (i) (not (= i pick))) v))
               (new_a (for/list ((vv filtered)) (list-ref a vv))))
           ;; Faster (but splice is inline)
           ;; (define new_a = a
            ;; new_a.splice(pick,1)
           (if (eq? val "w") 
               ; It was a whole pill: Add a half pill
               (f (append new_a (list "h")))
               ; Half pill: Just remove it
               (f new_a)))))
   
    (define init (rep n "w"))
    (define half (f init))
    (define h (harmonic_number n))
    (list half
          h
          )
   )
)

(show-marginals (model)
                (list  "half"
                       "h"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

(for ([n (range 1 11)])
  (show-marginals (model n)
                  (list  "n"
                         "half"
                         "h"
                         )
                  #:num-samples 10000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:hpd-interval (list 0.84)
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ; #:burn 0
                  ; #:thin 0
                  )
  )


