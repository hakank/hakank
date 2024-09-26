#| 

  Chess Tournament in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/117
  """
  A chess tournament has  K levels and N=2^K players with skills 
  P1 > P2 > ... > Pn. At each level, random pairs are formed and one person 
  from each pair proceeds to the next level. When two opponents play, the 
  one with the better skills always wins. What is the probability that players 
  P1 and P2 will meet in the final level?

  Answer:  n / (2* (n-1))
  """

  For k = 2..7:

  (k 2 n 4)
  var : final-pair
  (0 1): 0.6677
  (0 2): 0.3323

  var : theoretical
  2/3: 1.0
  mean: 0.6666666666666666

  (k 3 n 8)
  var : final-pair
  (0 1): 0.5746
  (0 2): 0.2877
  (0 3): 0.1107
  (0 4): 0.027

  var : theoretical
  4/7: 1.0
  mean: 0.5714285714285714

  (k 4 n 16)
  var : final-pair
  (0 1): 0.5317000000000001
  (0 2): 0.26920000000000005
  (0 3): 0.12400000000000001
  (0 4): 0.05000000000000001
  (0 5): 0.018600000000000002
  (0 6): 0.005300000000000001
  (0 7): 0.0010000000000000002
  (0 8): 0.00020000000000000004

  var : theoretical
  8/15: 1.0000000000000002
  mean: 0.5333333333333334

  (k 5 n 32)
  var : final-pair
  (0 1): 0.5159
  (0 2): 0.2576
  (0 3): 0.1236
  (0 4): 0.0589
  (0 5): 0.0244
  (0 6): 0.0114
  (0 7): 0.005
  (0 8): 0.0019
  (0 9): 0.0011
  (0 10): 0.0002

  var : theoretical
  16/31: 1.0
  mean: 0.5161290322580645

  (k 6 n 64)
  var : final-pair
  (0 1): 0.5121000000000001
  (0 2): 0.2569000000000001
  (0 3): 0.12020000000000003
  (0 4): 0.05570000000000001
  (0 5): 0.029700000000000008
  (0 6): 0.014300000000000004
  (0 7): 0.007300000000000002
  (0 8): 0.0020000000000000005
  (0 10): 0.0013000000000000002
  (0 9): 0.00030000000000000003
  (0 12): 0.00010000000000000003
  (0 11): 0.00010000000000000003

  var : theoretical
  32/63: 1.0000000000000002
  mean: 0.507936507936508

  (k 7 n 128)
  var : final-pair
  (0 1): 0.4938
  (0 2): 0.2573
  (0 3): 0.1302
  (0 4): 0.0601
  (0 5): 0.0312
  (0 6): 0.0149
  (0 7): 0.0069
  (0 8): 0.0032
  (0 9): 0.0013
  (0 10): 0.0006
  (0 11): 0.0003
  (0 13): 0.0001
  (0 14): 0.0001

  var : theoretical
  64/127: 1.0
  mean: 0.5039370078740157

  * The only k that enumerate can handle in reasonable time is k=2:

  k 2 n 4)
  var : final-pair
  (0 1): 2/3 (0.6666666666666666)
  (0 2): 1/3 (0.3333333333333333)

  var : theoretical
  2/3: 1 (1.0)
  mean: 2/3 (0.6666666666666666)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model k)
  (; enumerate ; #:limit 1e-01
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n (expt 2 k)) ; k levels

   (define skills (sort (draw-without-replacement n (range (* 2 n))) >))

   ;  (a b c d e f) -> (a b) (c d) e f)
   (define (make-random-pairs people)
     (make-tuples 2 (draw-without-replacement (length people) people )))

   (define (fight a b) (if (> (list-ref skills a) (list-ref skills b)) a b))
   
   (define (tournament left)
     (if (= (length left) 2)
         left
         (let* ([pairs (make-random-pairs left)]
                [winners (for/list ([pair pairs])
                           (fight (first pair) (second pair)))])
           (tournament winners)
           )))
   
   (define final-pair (sort (tournament (range n)) <))
   
   (list final-pair
         (/ n (* 2 (- n 1))))
   
   )
)

(for ([k (range 2 7)])
  (show2 "k" k "n" (expt 2 k))
  (show-marginals (model k)
                  (list  "final-pair"
                         "theoretical"
                         )
                  #:num-samples 10000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )
  )

