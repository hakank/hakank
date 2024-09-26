#| 

  Duelling cowboys in Racket.Gamble 

  From http://cs.ioc.ee/ewscs/2020/katoen/katoen-slides-lecture1.pdf
  """
  int cowboyDuel(float a, b) { // 0 < a < 1, 0 < b < 1
    int t := A [1] t := B; // decide who shoots first
    bool c := true;
    while (c) {
       if (t = A) {
          (c := false [a] t := B); // A shoots B with prob. a
       } else {
          (c := false [b] t := A); // B shoots A with prob. b
       }
    }
    return t; // the survivor
   }

   Claim: Cowboy A wins the duel with probability a / (a+b-a*b)
   """

   Model 2 gives the exact (theoretical) result.

   With probabilities a: 0.3 and b: 0.7

   (a: 0.3 b 0.7 theoretical 0.37974683544303794)

   Model 1:
   var : survivor
   B: 0.6166
   A: 0.3834

 
   Model 2:
   first-shooter: A
   var : survivor
   B: 0.620253164556962
   A: 0.379746835443038

   first-shooter: B
   var : survivor
   B: 0.9039487726787621
   A: 0.09605122732123804

   first-shooter: random
   var : survivor
   B: 0.786873599650254
   A: 0.21312640034974592


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (theoretical a b)
  (/ a (- (+ a b) (* a b)))
  )

;
; First model, straightward port of the model using set!
; Enumerate gives strange results (as usual with set! models)
;
(define (model1 a b)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice)
   
   ; int t := A [1] t := B; ; decide who shoots first
   (define c #t)
   (define A "A")
   (define B "B")
   (define t "A") ; A always starts
   (for ([i (in-naturals)]
         #:break (not c))
     (if (eq? t A)
         (if (flip a)
             (set! c #f) ; A shoots B: it's over
             (set! t B)  ; otherwise, it's B's turn
             )
         (if (flip b)
             (set! c #f) ; B shoots A: it's over
             (set! t A)) ; otherwise, it's A's turn
         )
     )
             
    (list t) ; the survivor
   

   )
)

(define a 0.3)
(define b 0.7)
(show2 "a:" a "b" b "theoretical" (theoretical a b))

(displayln "\nModel 1:")
(show-marginals (model1 a b)
                (list  "survivor"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )

;
; This is a port of the WebPPL model duelling_cowboys.wppl
;
(define (model2 first-shooter a b)
  ; We have to limit enumerate to not go through all variants
  (enumerate #:limit 1e-001
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler ; #:transition (slice)

   (define A "A")
   (define B "B")
   
   (define (shoot shooter)
     ; A shoots B with prob. a
     ; B shoots A with prob. b
     (define c (if (eq? shooter A) (flip a) (flip b))) ; Did the shooter succeed?
     (if c 
         ; Yes, the shooter succeeded. Return as the survivor.
         shooter
         ; Switch shooter and continue to the next round
         (shoot (if (eq? shooter A) B A))
         )
     )
   
   (define survivor 
     (if (eq? first-shooter "random") 
         (shoot (if (flip 0.5) "A" "B"))
         (shoot first-shooter)
         )
     )
         
   (list survivor) ; the survivor
   

   )
)

(displayln "\nModel 2:")
(for ([first-shooter '("A" "B" "random")])
  (show "first-shooter" first-shooter)
  (show-marginals (model2 first-shooter a b )
                  (list  "survivor"
                         )
                  #:num-samples 10000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.94
                  ; #:credible-interval2 0.94                
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )
  )


