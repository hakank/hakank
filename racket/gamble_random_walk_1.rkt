#| 

  Classical random walk in Racket.Gamble 

  From Gunnar Blom, Lars Holst, Dennis Sandell:
  "Problems and Snapshots from the World of Probability"
  Page 6f, Problem 1.5 Classical Random Walk 1, 
  cases a) Probability of absorption
        b) Expected number of steps until absorption
        c) Ruin problem

  The probability of moving to the left or right is 0.5 (symmetric
  random walk). The walk stops when reaching either -a or +b (
  a and are both positive integers).

  * a=10 b=10 (absorption and expected number of steps until absorption)

  var : length arr
  mean: 100.47880000000053

  var : lastPos
  mean: 0.020000000000002238

  var : lastPos = a
  mean: 0.4990000000000015

  var : lastPos = b
  mean: 0.5010000000000018

  Theoretical prob: (1/2 1/2)
  Theoretical length: 100


  * a=1 b=100 (ruin problem)

  var : length arr
  mean: 94.5625000000006

  var : lastPos
  mean: -0.08089999999999664

  var : lastPos = a
  mean: 0.9909000000000017

  var : lastPos = b
  mean: 0.00910000000000005

  Theoretical prob: (100/101 1/101)
  Theoretical length: 100


  This is a port of my WebPPL model random_walk_1.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;; Theoretical probability that we ends in (-a,b): 
(define (theoreticalProb a b)
  ; (b/(a+b),a/(a+b));
  (let ([ab (+ a b)])
    (list (/ b ab) (/ a ab))))

;; Theoretical length until absorption
(define (theoreticalLength a b)
  (* a b))

(define (model a b)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   ;; -a .. b
   
   (define (walk arr)
     (let* ([len (length arr)]
            [last-a (if (= len 0) 0 (last arr))])
       (if (or (= last-a (- a)) (= last-a b)) ; ruin or top score??
           arr
           (walk (append arr (list (+ last-a (uniform-draw (list -1 1)))))))))
   
   (define arr (walk '()))
   (define lastPos (last arr))
        
   (list ; arr
         (length arr)
         lastPos
         (= lastPos (- a))
         (= lastPos b)
            
         )

    )
        
)


(displayln "* a=10 b=10")
(show-marginals (model 10 10)
                (list  ; "arr"
                       "length arr"
                       "lastPos"
                       "lastPos = a"
                       "lastPos = b"
                     )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(displayln (format "Theoretical prob: ~a" (theoreticalProb 10 10)))
(displayln (format "Theoretical length: ~a" (theoreticalLength 10 10)))
(newline)

(displayln "* a=1 b=100")
(show-marginals (model 1 100)
                (list  ; "arr"
                       "length arr"
                       "lastPos"
                       "lastPos = a"
                       "lastPos = b"
                     )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

(displayln (format "Theoretical prob: ~a" (theoreticalProb 1 100)))
(displayln (format "Theoretical length: ~a" (theoreticalLength 1 100)))
(newline)
