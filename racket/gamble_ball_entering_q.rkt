#| 

  Ball entering Q in Racket.Gamble 

  https://medium.com/intuition/a-probability-challenge-93d9936b9ef0
  (Note: This page is not available)
  """
  How likely is the ball to enter Q?


                o              0
              |   |  
              / . \          1   2
             / . . \       3   4   5
            / . . . \     6  7   8   9
           / . . . . \  10 11 12  13  14  
           |P|Q|R|S|T|   P  Q  R   S   T 

  """

  Using enumerate we get all possible paths. The probability of entering Q is 0.25.

  var : pP
  #f: 0.9375
  #t: 0.0625
  mean: 0.0625

  var : pQ
  #f: 0.75
  #t: 0.25
  mean: 0.25

  var : pR
  #f: 0.625
  #t: 0.375
  mean: 0.375

  var : pS
  #f: 0.75
  #t: 0.25
  mean: 0.25

  var : pT
  #f: 0.9375
  #t: 0.0625
  mean: 0.0625


  In summary:     P     Q    R     S    T
               0.0625 0.25 0.375 0.25 0.0625

  I.e. a gaussian distribution.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


;; The paths
(define P 10)
(define Q 11)
(define R 12)
(define S 13)
(define T 14)
(define p '( (1 2) 
             (3 4) 
             (4 5) 
             (6 7) 
             (7 8) 
             (8 9) 
             ("P" "Q") 
             ("Q" "R") 
             ("R" "S") 
             ("S" "T") 
             ))


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

    ;; Get the next pin
   (define (next a)
     (let ([i (last a)]) ;;What"s the last/current pin?
       (if (member i '("P" "Q" "R" "S" "T" ))
           a
           (let ([t (list-ref p i)])
             ;; Get the next
             (next (append a (list (if (flip 0.5) (first t) (second t)))))))))
  
  (define a (next (list 0)))
    
   ; How likely is the ball to enter Q? (and P,R,S,T)
    
   (define pP (eq? (last a) "P"))
   (define pQ (eq? (last a) "Q"))
   (define pR (eq? (last a) "R"))
   (define pS (eq? (last a) "S"))
   (define pT (eq? (last a) "T"))
   
   (list a
         pP
         pQ
         pR
         pS
         pT
    )
    
   )
)

(show-marginals (model)
                (list  "a"
                       "pP"
                       "pQ"
                       "pR"
                       "pS"
                       "pT"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


