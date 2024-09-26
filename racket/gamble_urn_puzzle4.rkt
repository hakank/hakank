#| 

  Urn puzzle in Racket.Gamble 

  From Daniel Litt (Sep 2, 2024)
  https://x.com/littmath/status/1830404515788427575
  """
  You have 3 urns, each containing 100 balls. In the first two, 99 of the balls 
  are red and the last is green. In the third urn, all 100 balls are red. 
  You choose one of the urns at random and remove 99 randomly chosen balls from it; 
  they’re all red. The last is probably
  - Green 
  - Red
  - Equally likely
  """


  The last is probably red and the urn picked was probably the third urn.


  This is too slow for enumerate, so the importance sampler is used instead.
  Here are two model, the first is very slow but the second is much faster.

  * Model 1, importance-sampler (very slow for 10000 samples: about 45s)

  var : urn
  3: 0.9808
  2: 0.011
  1: 0.0082

  var : left
  red: 0.9808
  green: 0.019200000000000002

  The mh-sampler gives the following (22s) which is a little strange:
  
   Model 1
   var : urn
   3: 1.0
   mean: 3.0

   var : left
   red: 1.0

   - For n=10 and enumerate:
   var : urn
   3: 5/6 (0.8333333333333334)
   1: 1/12 (0.08333333333333333)
   2: 1/12 (0.08333333333333333)
   mean: 11/4 (2.75)

   var : left
   red: 5/6 (0.8333333333333334)
   green: 1/6 (0.16666666666666666)


  * Model 2, using permutation-dist and vector is much faster: 3s
  var : urn
  3: 0.9798
  1: 0.0106
  2: 0.0096
  mean: 2.9692000000000003

  var : left
  red: 0.9798
  green: 0.0202



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;
; (remove-many x n)
; Remove n random elements from list x.
; Returns
;   (left removed)
;
(define (remove-many x n)
  (define (loop x n removed)
    (cond
      [(or (= n 0) (empty? x)) (list x (reverse removed))]
      [else (let* ([len (length x)]
                   [i (random-integer len)]
                   [v (list-ref x i)])
              (loop (remove v x) (sub1 n) (cons v removed)))]
      )
    )
  (loop x n '())
  )

(define (model1)
  (; enumerate ; #:limit 1e-01
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (enumerative-gibbs)

   (define num-urns 3)

   (define total-balls 100)
   (define t (sub1 total-balls)) 
   
   (define colors (vector "red" "green"))
   
   (define urn (add1 (random-integer num-urns)))

   ; Urn 1: 99 red balls, 1 green
   ; Urn 2: 99 red balls, 1 green
   ; Urn 3: 100 red balls   
   (define balls-in-urn
     (case urn
       [(1) (cons "green" (ones-list t "red"))]
       [(2) (cons "green" (ones-list t "red"))]
       [(3) (ones-list total-balls "red")]
       ))

   ; Remove 99 ball and observe that all are red
   ; what color is the last ball?
   (define (remove-and-observe-red x n)
     (define (loop x n)
       (cond
         [(or (= n 0) (empty? x)) (first x)]
         [else (let* ([len (length x)]
                      [i (random-integer len)]
                      [v (list-ref x i)])
                 (observe/fail (eq? v "red"))
                 ; (observe-sample (dist-unit v) "red")
                 (loop (remove v x) (sub1 n)))]
         )
       )
     (loop x n) 
     )
   
   (define left (remove-and-observe-red balls-in-urn t))
   
   (list urn
         left
         )

   )
)

#|
(displayln "Model 1")
(show-marginals (model1)
                (list  "urn"
                       "left"
                     )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
|#

;;
;; Alternative, using permutation and vector. Faster
;;
(displayln "\nModel 2")
(define (model2)
  (; enumerate ; #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num-urns 3)

   (define colors (vector "red" "green"))
   
   (define urn (add1 (random-integer num-urns)))

   ; Urn 0: 99 red balls, 1 green
   ; Urn 1: 99 red balls, 1 green
   ; Urn 2: 100 red balls
   (define balls-in-urn
     (case urn
       [(1) (cons "green" (ones-list 99 "red"))]
       [(2) (cons "green" (ones-list 99 "red"))]
       [(3) (ones-list 100 "red")]
       ))


   ; Randomly permute the 100 balls.
   ; Observe that the first 99 are red.
   ; What is the color of the 100th ball?
   (define left (let ([p (sample (permutation-dist 100))])
               (for ([i 99])
                 (observe-sample (dist-unit (ref-2d balls-in-urn p i)) "red")
                 )
               (ref-2d balls-in-urn p 99))
     )
   

   (list urn
         left
         )

   )
  )

(show-marginals (model2)
                (list  "urn"
                       "left"
                     )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


