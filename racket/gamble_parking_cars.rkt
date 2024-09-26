#| 

  Parking cars in Racket.Gamble 

  From https://www.reddit.com/r/Probability/comments/1f4dd3j/parking_cars/
  """
  Parking Cars

  I've been thinking through this probability question that has left me a little confused and 
  was wondering if there was anyone here who could help point me in the right direction. It goes 
  like this: 
  There are 10 spots in a parking lot arranged in a single row. Three cars are parked 
  randomly. What is the probability that none of these cars are in adjacent spots?
  """

  var : num-adjacent
  0: 7/15 (0.4666666666666667)
  1: 7/15 (0.4666666666666667)
  2: 1/15 (0.06666666666666667)
  mean: 3/5 (0.6)

  Note: num-adjacent here means number of adjacent pairs. This, for n parked cars,
  there are n-1 adjacent pairs.


  Here are the solutions 1..10 parked cars (for 10 parking spots):

  num-to-park: 1
  var : num-adjacent
  0: 1 (1.0)
  mean: 0 (0.0)

  num-to-park: 2
  var : num-adjacent
  0: 4/5 (0.8)
  1: 1/5 (0.2)
  mean: 1/5 (0.2)

  num-to-park: 3
  var : num-adjacent
  0: 7/15 (0.4666666666666667)
  1: 7/15 (0.4666666666666667)
  2: 1/15 (0.06666666666666667)
  mean: 3/5 (0.6)

  num-to-park: 4
  var : num-adjacent
  1: 1/2 (0.5)
  2: 3/10 (0.3)
  0: 1/6 (0.16666666666666666)
  3: 1/30 (0.03333333333333333)
  mean: 6/5 (1.2)

  num-to-park: 5
  var : num-adjacent
  2: 10/21 (0.47619047619047616)
  1: 5/21 (0.23809523809523808)
  3: 5/21 (0.23809523809523808)
  0: 1/42 (0.023809523809523808)
  4: 1/42 (0.023809523809523808)
  mean: 2 (2.0)

  num-to-park: 6
  var : num-adjacent
  3: 10/21 (0.47619047619047616)
  2: 5/21 (0.23809523809523808)
  4: 5/21 (0.23809523809523808)
  1: 1/42 (0.023809523809523808)
  5: 1/42 (0.023809523809523808)
  mean: 3 (3.0)

  num-to-park: 7
  var : num-adjacent
  4: 1/2 (0.5)
  5: 3/10 (0.3)
  3: 1/6 (0.16666666666666666)
  6: 1/30 (0.03333333333333333)
  mean: 21/5 (4.2)

  num-to-park: 8
  var : num-adjacent
  5: 7/15 (0.4666666666666667)
  6: 7/15 (0.4666666666666667)
  7: 1/15 (0.06666666666666667)
  mean: 28/5 (5.6)

  num-to-park: 9
  var : num-adjacent
  7: 4/5 (0.8)
  8: 1/5 (0.2)
  mean: 36/5 (7.2)

  num-to-park: 10
  var : num-adjacent
  9: 1 (1.0)
  mean: 9 (9.0)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model [num-to-park 3])
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define num-cars 10)
   ; (define num-to-park 3)

   (define cars (draw-without-replacement num-to-park (range num-cars)))
   (define cars-sorted (sort cars <))

   (define num-adjacent (for/sum ([i (range 1 num-to-park)])
                          (if (= (- (list-ref cars-sorted i) (list-ref cars-sorted (sub1 i))) 1) 1 0)))

   (list num-adjacent
         )
   
   )
)


(show-marginals (model)
                (list  "num-adjacent"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(for ([num-to-park (range 1 11)])
  (displayln (format "num-to-park: ~a" num-to-park))
  (show-marginals (model num-to-park)
                  (list  "num-adjacent"
                         )
                  #:num-samples 100000
                  #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )
  )


