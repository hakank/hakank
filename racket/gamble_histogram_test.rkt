#| 

  Test of histogram and percentiles Racket Gamble.
  
  This is an example of the parameters to show-marginals
    #:show-histogram?
    #:show-percentiles?

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")



(define (model)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define x (normal 100 15))
   ; (define x (poisson 100))
   ; (define x (poisson 10))   
   (define x (flip 0.4))
   ; (define x (categorical-vw2 (vector 1 2 3 4 5) (vector "a" "b" "c" "d" "e")))
   ; (define x (categorical-vw2 (vector 1 2 3 4 5) (vector 'a 'b 'c 'd 'e)))

   ; (define y (normal 0 .1))
   ; (define y (poisson 100))
   (define y (poisson 10))   
   ; (define y (flip 0.5))
   ; (define y (binomial 4 0.5))
   ; (define y (categorical-vw2 (vector 1 2 3 4 5) (vector "a" "b" "c" "d" "e")))
   ; (define y (categorical-vw2 (vector 1 2 3 4 5) (vector 'a 'b 'c 'd 'e)))
   ; (define y (categorical-vw2 (vector 1 2 3 4) (vector '(0 0) '(0 1) '(1 0) '(1 1))))
   
   ; (show2 "x" x "y" y)
   
   (list x y
         )
  
   )
  )

;; (newline)
(show-marginals (model)
                (list "x"
                      "y"
                      )
                #:num-samples 1000
                #:truncate-output 4
                ; #:skip-marginals? #t
                #:credible-interval 0.94
                #:credible-interval2 0.94
                #:show-stats? #t
                #:show-histogram? #t ; 10 ; #t
                ; #:show-percentiles? #t ; '(0.01 0.2 0.5 0.7 0.99)  ; #t
                #:show-percentiles? '(0.01 0.2 0.5 0.7 0.94 0.99 0.99999)  ; #t                
                )

