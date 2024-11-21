#| 

  Runs in Racket/Gamble 

  Some experiments with runs.

  Here is an example of runs-loop on a binary list of 200 elements. 
  This evaluates the given list with get-runs until the list is settled (does not change).

  runs-loop: (1 1 1 1 0 1 1 1 0 0 1 0 0 0 1 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 1 0 1 1 1 0 0 0 1 1 0 0 0 1 0 0 1 0 0 0 1 1 1 1 1 1 0 1 0 1 1 1 0 0 1 0 0 1 1 1 1 0 1 1 1 1 0 0 0 1 1 0 1 1 0 1 1 1 0 0 1 1 0 1 0 0 0 0 1 0 1 1 1 0 0 0 1 1 0 1 1 0 1 1 1 0 0 1 1 1 0 0 1 1 1 1 0 1 1 0 1 0 1 0 0 1 0 1 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0 1 1 1 1 1 1 0 0 1 1 0 0 0 0 1 0 0 1 0 0)
  runs-loop: (4 1 3 2 1 3 1 1 5 9 3 2 1 2 3 2 1 1 3 3 2 3 1 2 1 3 6 1 1 1 3 2 1 2 4 1 4 3 2 1 2 1 3 2 2 1 1 4 1 1 3 3 2 1 2 1 3 2 3 2 4 1 2 1 1 1 1 2 1 1 3 5 1 4 1 7 1 3 1 1 2 1 6 2 2 4 1 2 1 2)
  runs-loop: (1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 2 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 2 2 1 1 1 1 1 1 1 1 1 1 1 4 1 2 1 1 1 1 1 1 1 1 2 1 1 1 2 1 1 1 1 1)
  runs-loop: (6 1 8 2 7 1 13 2 1 2 11 1 1 1 8 1 3 1 5)
  runs-loop: (1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1)
  runs-loop: (11 1 5)
  runs-loop: (1 1 1)
  runs-loop: (3)
  runs-loop: (1)
  '((1) 8)

  I.e. it took 8 iterations. And it seems to always end with a single 1.


  Also see gamble_streaks.rkt

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


; Loop until the runs of xs are "settled", normally a single number
; Returns the last xs and the number of iterations
(define (runs-loop xs [trace? #f])
  (define (loop a i)
    (when trace? (show "runs-loop" a))
    (let ([new-a (get-runs-lens a)])
      (if (equal? a new-a)
          (list a i)
          (loop new-a (add1 i)))))
  (loop xs 0)
  )


(define *throws* (for/list ([i 200]) (random-integer 2)))
*throws*
(newline)
(get-runs-lens *throws*)
(runs-loop *throws* #t)
(newline)

#|

  Number of iterations for a repeated run on a binary list

  * For n=10, enumerate gives the following

  variable : last-run
  (1): 1 (1.0)

  variable : num-iterations
  6: 95159/262144 (0.3630027770996094)
  7: 44163/131072 (0.33693695068359375)
  8: 85931/524288 (0.16390037536621094)
  4: 33333/524288 (0.06357765197753906)
  5: 15099/262144 (0.057598114013671875)
  9: 3925/262144 (0.014972686767578125)
  3: 5/524288 (9.5367431640625e-6)
  2: 1/524288 (1.9073486328125e-6)
  mean: 3420909/524288 (6.524866104125977)

  * For n=200 (importance-sampler, 100000 samples)
  variable : last-run
  (1): 1.0000000000000002

  variable : num-iterations
  10: 0.35030000000000006
  11: 0.27459000000000006
  9: 0.15818000000000002
  8: 0.14559000000000002
  12: 0.06882000000000002
  13: 0.0010000000000000002
  6: 0.0009700000000000002
  7: 0.0005500000000000001
  mean: 9.96034


  * For n=1000
  variable : last-run
  (1): 1.0000000000000002

  variable : num-iterations
  13: 0.32830000000000004
  12: 0.26570000000000005
  11: 0.21370000000000003
  14: 0.16550000000000004
  15: 0.013200000000000002
  10: 0.008700000000000001
  9: 0.004900000000000001
  mean: 12.453100000000003


  * For n=10 and random values 0..2, enumerate:
  variable : last-run
  (1): 1 (1.0)

  variable : num-iterations
  5: 7280/19683 (0.36986231773611744)
  6: 7024/19683 (0.356856170299243)
  7: 1088/6561 (0.16582837982014936)
  4: 1456/19683 (0.07397246354722349)
  3: 530/19683 (0.026926789615404154)
  8: 128/19683 (0.00650307371843723)
  2: 1/19683 (5.080526342529086e-5)
  mean: 109832/19683 (5.580043692526545)


|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 200)
   (define m 2)
   ; (define m 2)
   ; (define throws (for/list ([i n]) (random-integer 2)))
   (define throws (for/list ([i n]) (random-integer m)))   
   (define res (runs-loop throws))   
   (define last-run (first res))
   (define num-iterations (second res))
   
   (list last-run
         num-iterations
         )
   )
)

(show-marginals (model)
                (list  "last-run"
                       "num-iterations"
                     )
                    #:num-samples 100000
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


