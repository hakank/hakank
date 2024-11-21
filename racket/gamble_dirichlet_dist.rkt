#| 

  Dirichlet dist (generating) in Racket/Gamble 

  https://en.wikipedia.org/wiki/Dirichlet_distribution#Random_variate_generation

  Example: Given a list of random values (0.0..1.0) that sums to 1,
  what is the probability that the first value is larger than the sum
  of the second and the third values.
  Answer: 0.25

  It does not matter how many values there are in the list.
  Here's a run with 3..10 values and alpha (parameters) of 1s
  
  n: 3
  variable : p
  mean: 0.25150000000000483
 
  n: 4
  variable : p
  mean: 0.24790000000000523

  n: 5
  variable : p
  mean: 0.2482500000000052

  n: 6
  variable : p
  mean: 0.24835000000000518

  n: 7
  variable : p
  mean: 0.250200000000005

  n: 8
  variable : p
  mean: 0.24950000000000505

  n: 9
  variable : p
  mean: 0.24855000000000516

  n: 10
  variable : p
  mean: 0.2472500000000053

  But this changes for other alpha values. Here we have n=3 and 
  different parameter values (e.g. all 1s, all 10s, etc)

  (n 3 param-val 1)
  variable : p
  mean: 0.24955000000000505

  (n 3 param-val 10)
  variable : p
  mean: 0.032450000000003275

  (n 3 param-val 20)
  variable : p
  mean: 0.004000000000000401

  (n 3 param-val 50)
  variable : p
  mean: 0 (0.0)

  (n 3 param-val 100)
  variable : p
  mean: 0 (0.0)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (dirichlet_dist params)
  (let* ([samples (for/list ([p params]) (gamma p 1))]
         [s (sum samples)]
         )
    (map (lambda (v) (/ v s)) samples)))

(dirichlet_dist (list 1 1 1 1))


(define (model n param-val)
  (show2 "n" n "param-val" param-val)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define n 3)
   (define params (ones-list n param-val))
   (define d (dirichlet_dist params))

   (define p (> (first d) (+ (second d) (third d))))
   
   (list d p)
   
   )  
)

(for ([n (range 3 11)])
  (show-marginals (model n 1)
                (list  "d"
                       "p"
                       )
                #:num-samples 20000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )

)

(displayln "\nDifferent parameter values, n=3")
(for ([p-val '(1 10 20 50 100)])
  (show-marginals (model 3 p-val)
                  (list  "d"
                         "p"
                         )
                  #:num-samples 20000
                  #:truncate-output 5
                  #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:hpd-interval (list 0.84)
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ; #:burn 0
                  ; #:thin 0
                  )
  
)
