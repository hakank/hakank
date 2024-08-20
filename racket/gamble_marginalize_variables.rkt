#| 

  Marginalize variables in Racket Gamble.

  Here are some tests of (show-marginals), i.e. to show the marginal probabilities in
  a model with multiple return variables. This is inspired by how WebPPL presents
  the output.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

; From gamble_binomial_basketball.rkt
;; Combine the questions 
;; ((#f #f) : 0.5730438232421875)
;; ((#t #f) : 0.41816711425781244)
;; ((#f #t) : 0.0050811767578125035)
;; ((#t #t) : 0.0037078857421875048)


(displayln "Combined values ")
(define (model)
  (
   rejection-sampler
   
   (define p 0.75)
   
   (define b1 (binomial 3 p))
   (define b2 (binomial 10 p))

   ; Skipping this since it makes the output to big
   ; (and it's not used...)
   ; (define pois (poisson 3))
   
   (observe/fail (< b2 8))
   
   (define prob (> b1 b2))
   
   (list b1 b2 prob)
     
   )
  )

(show-model (model) #:no-stats? #t #:no-cred? #t)
(newline)

; Just playing
(define (model2)
  (enumerate ; To slow when using pois
   ; rejection-sampler
   
   (define p 0.75)
   
   (define b1 (binomial 3 p))
   (define b2 (binomial 10 p))

   ; Just testing
   ; (define pois (poisson 3))
   
   (observe/fail (< b2 8))
   
   (define prob (> b1 b2))

   ; (list b1 b2 prob pois)
   (list b1 b2 prob)   
   )
  )

;;
;; Here is one way showing the individual marginals for each variable,
;; but it's too messy.
;;
(define (model2-compare var)
  (show "var" var)
  (enumerate ; Too slow when using pois
   ; rejection-sampler
   
   (define p 0.75)
   
   (define b1 (binomial 3 p))
   (define b2 (binomial 10 p))

   ; Just testing
   ; (define pois (poisson 3))
   
   (observe/fail (< b2 8))
   
   (define prob (> b1 b2))
   
   (case var
     [("b1") b1]
     [("b2") b2]
     [("prob") prob]
     ; [("pois") pois]
     )
   )
  )


; (show-model (model2))


; (show-marginals (model2) (list "b1" "b2" "prob" "pois") #:num-samples 10000)
; Skipping pois
(show-marginals (model2) (list "b1" "b2" "prob") #:num-samples 10000)

(displayln "\ncompare:")

(show-model (model2-compare "b1"))
(show-model (model2-compare "b2"))
(show-model (model2-compare "prob"))
; (show-model (model2-compare "pois"))

;;
;; Using continuous variables
;;
(define (model3)
  (
   ; rejection-sampler ; Does not support observe-sample
   importance-sampler
   ; mh-sampler
   
   (define p (beta 1 1))
   (define b (binomial 10 p))
   
   (define (t i) (if (< b 3)
                 (normal-dist 0 1)
                 (normal-dist 100 15)))
     
   
   (observe-sample (t 0) 100.43)
   (observe-sample (t 1) 95.43)
   (observe-sample (t 2) 123.8)
   (observe-sample (t 3) 13.4)
   (observe-sample (t 4) 1.4)

   (list (> p 0.5) b)
   )
  )

(displayln "\nmodel3")
;; (sampler->discrete-dist (model3) 1000)
(show-marginals (model3) (list "p>0.5" "b") #:num-samples 10000)


;; Test with symbols/string/list variables
(define (model4)
  (enumerate
   ; rejection-sampler ; Does not support observe-sample
   ; importance-sampler
   ; mh-sampler
   
   ;; (define x (uniform-draw (list "a" "b" "c")))
   ;; (define y (uniform-draw (list "a" "b" "c")))
   (define x (uniform-draw (list 'a 'b 'c)))
   (define y (uniform-draw (list 'a 'b 'c 'd 'e)))
   (define p1 (eq? x y))
   
   ; (define p2 (string>? x y))
   (define p2
     (if (symbol? x )
         (flip 0.3)
         (string>? x y)))
   
   (define p1-and-p2 (list p1 p2))
   
   (list x y p1 p2 p1-and-p2)
   )
  )

(displayln "\nmodel4 with strings/symbols/lists")
;; (sampler->discrete-dist (model3) 1000)
(show-marginals (model4) (list "x" "y" "x=y" "x>y" "p1-and-p2") #:num-samples 10000)
