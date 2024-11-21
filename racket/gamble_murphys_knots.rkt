#| 

  Murphy's knots (Random walks in n dimensions) in Racket/Gamble 

  Robert Matthews has written an article about knots on a string 
  'Knotted rope: a topological example of Murphy's Law' ("If rope can become knotted, it will do").
  Matthews mentions a random walk in 3 dimensions.

  The simulation consists of the following.
 
  - The dimension is n
  - Start from position (0,0,0) (i.e. n 0s)
  - Get a new random position, one step in some of the directions, i.e. -1 or 1 in either 
    x, y, and z direction (for dimension n=3).
  - The criteria that it's a knot is that one come back some of the visited positions.
  - The unknown parameter is the length of the path until a knot occurs somewhere in the
    string. Note that the last point is included in the path, which means that the
    minimal value is 3 for all dimensions.

  * for n = 3
  variable : len
  3: 0.16760000000000005
  4: 0.14080000000000006
  5: 0.13190000000000004
  6: 0.10310000000000002
  7: 0.09290000000000002
  8: 0.07310000000000003
  9: 0.06520000000000002
  10: 0.04710000000000002
  11: 0.03630000000000001
  12: 0.02920000000000001
  13: 0.025400000000000006
  14: 0.019300000000000008
  15: 0.014500000000000006
  16: 0.011400000000000004
  17: 0.011400000000000004
  18: 0.0069000000000000025
  19: 0.005800000000000001
  22: 0.0031000000000000008
  20: 0.0029000000000000007
  21: 0.002500000000000001
  23: 0.002200000000000001
  26: 0.0021000000000000007
  25: 0.0010000000000000005
  24: 0.0009000000000000003
  27: 0.0008000000000000003
  29: 0.0005000000000000002
  31: 0.00040000000000000013
  30: 0.0003000000000000001
  34: 0.0003000000000000001
  35: 0.0003000000000000001
  28: 0.00020000000000000006
  33: 0.00020000000000000006
  37: 0.00020000000000000006
  32: 0.00010000000000000003
  42: 0.00010000000000000003
  mean: 7.250900000000005

  * for n=1..10
  n: 1
  variable : len
  mean: 4.0125

  n: 2
  variable : len
  mean: 5.5885000000000025

  n: 3
  variable : len
  mean: 7.280400000000002

  n: 4
  variable : len
  mean: 9.164400000000006

  n: 5
  variable : len
  mean: 11.014400000000004

  n: 6
  variable : len
  mean: 12.935300000000005

  n: 7
  variable : len
  mean: 14.998900000000006

  n: 8
  variable : len
  mean: 17.18070000000002

  n: 9
  variable : len
  mean: 19.28250000000001

  n: 10
  variable : len
  mean: 20.993800000000032


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model [n 3])
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define n 3) ; dimension
   (define dirs '(-1 1))
   (define max_steps 100)

   (define (step s steps)
     (if (>= s max_steps)
         steps
         (let* ([last_step (last steps)]
                [coord (random-integer n)] ; which direction
                [dir (categorical-vw2 (vector 1 1) (vector -1 1))]
                [new_step (for/list ([i n])
                            (let ([last_step_i (list-ref last_step i)])
                              (if (= i coord) 
                                  (+ last_step_i dir)
                                  last_step_i)))]
                [new_steps (append steps (list new_step))]
                [seen (check-duplicates new_steps equal?)])
           (if seen
               new_steps
               (step (add1 s) new_steps)))))

   (define a (step 0 (list (ones-list n 0))))
   (define len (length a))

   (list ; a
         len
         )
    
   )
)

(show-marginals (model 3)
                (list  ; "a"
                       "len"
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

(newline)
(for ([n (range 1 11)])
  (show "n" n)
  (show-marginals (model n)
                (list  ; "a"
                       "len"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
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
