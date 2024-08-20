#| 

  Chicken pecking problem in Racket Gamble.

  From (PSI model) 
  "HACSTFB - Probabilistic Programming Language ψ"
  https://www.youtube.com/watch?v=COS5YThEm1A
  About @10:04 ff

  10 chicken are arranged in a ring. With probability 1/2 a chicken picks the 
  chicken at left and probability 1/2 at right,
  What is the probability of unpecked chickens?

  From the PSI model, probabilities of number of unpecked chicken = 0,1,2,3,4,5,6,7,8,9,10;
  E[r1_,r2_,r3_,r4_,r5_,r6_,r7_,r8_,r9_,r10_,r11_] = (1/256,5/64,55/128,25/64,25/256,0,0,0,0,0,0)
  (0.00390625,0.078125,0.4296875,0.390625,0.09765625,0,0,0,0,0,0)

  This is a port of my WebPPL model chicken_pecking.wppl

  This matched the exact PSI model (as well as the WebPPL model)
  (2 : 55/128 (0.4296875))
  (3 : 25/64 (0.390625))
  (4 : 25/256 (0.09765625))
  (1 : 5/64 (0.078125))
  (0 : 1/256 (0.00390625))
  (mean: 2.5)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (chicken-pecking n)
  (enumerate
   
   ; This chicken picks (randomly) on either its left or right neighbour
   (define pecked-who
     (for/list ([i (range n)])
       (modulo (+ i (if (flip) (- 1) 1)) n)
       ))
   
   ; number of unpecked chickens
   (define unpecked
     (for/sum ([c1 (range n)]) 
               ; Number of chicken that picked on chicken c1
               (if (= 0 (for/sum([c2 (range n)])
                                 (if (= (list-ref pecked-who c2) c1) 1 0)
                                 )) 1 0)
               ))
   
   unpecked
   )
  )


(show-model (chicken-pecking 10))


;;
;; This was inspired by the PSI model chicken_pecking.psi
;; Note: it seems that enumerate does not like vector-set! and set!.
;;
;; (2 : 0.4265)
;; (3 : 0.3892)
;; (4 : 0.0978)
;; (1 : 0.0819)
;; (0 : 0.0046)
;; (mean: 2.4937)
;; Min: 0 Mean: 2.5011 Max: 4 Variance: 0.61699879 Stddev: 0.7854927052493867
;; Credible interval (0.84): 1..3
;;
(define (chicken-pecking2 n)
  (; enumerate
   rejection-sampler
   ; importance-sampler

   ; Initialize all chickens to be unpecked
   (define pecked (make-vector n #f))

   ; Flag the chicken that was picked upon
   (for ([i (range n)])
     (vector-set! pecked (modulo (+ i (if (flip) (- 1) 1)) n) #t )
     )
   
   ; Update the value of unpecked chickens
   ; Using set! just to show that it works (for the samplers)
   (define unpecked 0)
   (for ([c (range n)])
     (when (eq? (vector-ref pecked c) #f)
       (set! unpecked (add1 unpecked))))

   ;; This is better, but it does not use set! which was my point to show...
   ;; (define unpecked
   ;;   (for/sum ([v pecked]) (if v 0 1))
   ;;   )
   
   ; (show "unpecked" unpecked)
   unpecked
   )
  )



(show-model (chicken-pecking2 10)
            #:num-samples 10000)
