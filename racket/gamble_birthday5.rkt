#| 

  Birthday paradox in Racket/Gamble 


  Here I'm trying with a submodel. It works but it's very slow.
  
  For example for #:num-samples 20 in the main model and 20 samples in the sub model, it takes almost 20s:

variable : n
22: 0.25
25: 0.15
19: 0.1
23: 0.1
24: 0.1
28: 0.1
29: 0.1
18: 0.05
20: 0.05
mean: 23.450000000000003


  With #:num-samples 1000:
variable : n
23: 0.10499999999999997
21: 0.08799999999999998
24: 0.08799999999999998
20: 0.07999999999999999
19: 0.07699999999999999
22: 0.07499999999999998
25: 0.06399999999999999
26: 0.06299999999999999
27: 0.05799999999999999
18: 0.054999999999999986
28: 0.052999999999999985
17: 0.041999999999999996
29: 0.039999999999999994
30: 0.034999999999999996
16: 0.025999999999999992
15: 0.021999999999999995
14: 0.012999999999999996
13: 0.010999999999999998
12: 0.002999999999999999
10: 0.0009999999999999998
11: 0.0009999999999999998
mean: 22.417999999999992

variable : ddv
(0.5 0.5): 0.9999999999999997



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define days 365)

   ; Generate n number of people in the room
   (define n (add1 (random-integer 30)))
   
   ; Generate birthdays for the n peope in the room
   ; and return the probabiity that at least two person
   ; have the same birthday
   (define (sub-model)
     (; enumerate
      ; rejection-sampler
      importance-sampler
      ; mh-sampler
      (define people (for/list ([i (range n)]) (random-integer days)))
      ; at least one duplicate: at least two with the same birthday
      (< (length (remove-duplicates people)) n)
      ; (check-duplicates people)
     
     ))

   ;; Another approach: Using recursion instead for earlier failure.
   (define (sub-model2)
     (enumerate
      ; rejection-sampler
      ; importance-sampler
      ; mh-sampler

      (define (f a)
        (let ([len (length a)])
          (if (> len n)
              #f
              (let ([birthday (random-integer days)])
                (if (member? birthday a)
                    #t
                    (f (append a (list birthday))))))))
      (f '())
     
      ))
   
   (define (sub-model3)
     (; enumerate
      ; rejection-sampler
      importance-sampler
      ; mh-sampler

      (define (f a)
        (let ([len (length a)])
          (if (> len n)
              #f
              (if (check-duplicates a)
              ; (if (has-dup?/hash-eqv a)
                  #t
                  (f (append a (list (random-integer days))))))))
        (f '())
     
     ))

   
   ;; Run the sub-model a couple of times and ensure that the
   ;; probability is (exactly/about) 50/50.
   (define s (sampler->discrete-dist (sub-model) 20)) ; Even number (otherwise it cannot be 0.5/0.5)
   ; (define s (sampler->discrete-dist (sub-model2) 20)) ;
   ; (define s (sampler->discrete-dist (sub-model3) 20)) ;
   (define ddv (vector->list (discrete-dist-weights s)))
   (observe/fail (equal? ddv '(0.5 0.5)))

   ;; A little less strict test
   ;; (observe/fail (eq? 2 (length ddv)))
   ;; (define diff (abs (-  (first ddv) (second ddv))))
   ;; (observe/fail (<= diff 0.05)) 

   (show2 "n" n "ddv" ddv)

   (list n ddv)
   
   )
)

(show-marginals (model)
                (list  "n"
                       "ddv"
                     )
                    #:num-samples 20
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


