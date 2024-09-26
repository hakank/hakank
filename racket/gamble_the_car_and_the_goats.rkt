#| 

  The Car and the Goats (Monty Hall) in Racket.Gamble 

  From  Gunnar Blom, Lars Holst, Dennis Sandell:
  "Problems and Snapshots from the World of Probability"
  Page 3f, Problem 1.3 The car and the goats

  This first part of the problem is a "traditional" analysis of 
  the Monty Hall problem. See gamble_monty_hall.rkt for a model 
  of this problem.

  The second part "A generalization" is a urn approach which
  this Gamble model implements.

  Which of these strategies is best for A:
  * strategy 1
    """
    A draws a ball at random. If it is white, he wins the game,
    otherwise he loses.
    """
    This corresponds to the Stay strategy.

  * strategy 2
    """
    A draws a ball and throws it away. B removes a black ball.
    A again draws a ball. If it is white, he wins the game,
    otherwise he loses.
    """
    This corresponds to the Switch strategy.

  Here we test some different number of white (a) and black (b) balls.
  The first (a=1, b=2) is the traditional Monty Hall problem.


* a: 1 b: 2
Theoretical probability of winning for each strategy: (1/3 2/3)
var : ball1
b: 2/3 (0.6666666666666666)
w: 1/3 (0.3333333333333333)

var : win1
#f: 2/3 (0.6666666666666666)
#t: 1/3 (0.3333333333333333)
mean: 1/3 (0.3333333333333333)

var : ball2
w: 2/3 (0.6666666666666666)
b: 1/3 (0.3333333333333333)

var : win2
#t: 2/3 (0.6666666666666666)
#f: 1/3 (0.3333333333333333)
mean: 2/3 (0.6666666666666666)

var : s2-beats-s1
#f: 5/9 (0.5555555555555556)
#t: 4/9 (0.4444444444444444)
mean: 4/9 (0.4444444444444444)


* a: 1 b: 3
Theoretical probability of winning for each strategy: (1/4 3/8)
var : ball1
b: 3/4 (0.75)
w: 1/4 (0.25)

var : win1
#f: 3/4 (0.75)
#t: 1/4 (0.25)
mean: 1/4 (0.25)

var : ball2
b: 5/8 (0.625)
w: 3/8 (0.375)

var : win2
#f: 5/8 (0.625)
#t: 3/8 (0.375)
mean: 3/8 (0.375)

var : s2-beats-s1
#f: 23/32 (0.71875)
#t: 9/32 (0.28125)
mean: 9/32 (0.28125)


* a: 1 b: 10
Theoretical probability of winning for each strategy: (1/11 10/99)
var : ball1
b: 10/11 (0.9090909090909091)
w: 1/11 (0.09090909090909091)

var : win1
#f: 10/11 (0.9090909090909091)
#t: 1/11 (0.09090909090909091)
mean: 1/11 (0.09090909090909091)

var : ball2
b: 89/99 (0.898989898989899)
w: 10/99 (0.10101010101010101)

var : win2
#f: 89/99 (0.898989898989899)
#t: 10/99 (0.10101010101010101)
mean: 10/99 (0.10101010101010101)

var : s2-beats-s1
#f: 989/1089 (0.9081726354453628)
#t: 100/1089 (0.09182736455463728)
mean: 100/1089 (0.09182736455463728)


* a: 2 b: 10
Theoretical probability of winning for each strategy: (1/6 11/60)
var : ball1
b: 5/6 (0.8333333333333334)
w: 1/6 (0.16666666666666666)

var : win1
#f: 5/6 (0.8333333333333334)
#t: 1/6 (0.16666666666666666)
mean: 1/6 (0.16666666666666666)

var : ball2
b: 49/60 (0.8166666666666667)
w: 11/60 (0.18333333333333332)

var : win2
#f: 49/60 (0.8166666666666667)
#t: 11/60 (0.18333333333333332)
mean: 11/60 (0.18333333333333332)

var : s2-beats-s1
#f: 61/72 (0.8472222222222222)
#t: 11/72 (0.1527777777777778)
mean: 11/72 (0.1527777777777778)


* a: 2 b: 2
Theoretical probability of winning for each strategy: (1/2 3/4)
var : ball1
w: 1/2 (0.5)
b: 1/2 (0.5)

var : win1
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)

var : ball2
w: 3/4 (0.75)
b: 1/4 (0.25)

var : win2
#t: 3/4 (0.75)
#f: 1/4 (0.25)
mean: 3/4 (0.75)

var : s2-beats-s1
#f: 5/8 (0.625)
#t: 3/8 (0.375)
mean: 3/8 (0.375)


* a: 10 b: 10
Theoretical probability of winning for each strategy: (1/2 19/36)
var : ball1
w: 1/2 (0.5)
b: 1/2 (0.5)

var : win1
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)

var : ball2
w: 19/36 (0.5277777777777778)
b: 17/36 (0.4722222222222222)

var : win2
#t: 19/36 (0.5277777777777778)
#f: 17/36 (0.4722222222222222)
mean: 19/36 (0.5277777777777778)

var : s2-beats-s1
#f: 53/72 (0.7361111111111112)
#t: 19/72 (0.2638888888888889)
mean: 19/72 (0.2638888888888889)


* a: 10 b: 2
Theoretical probability of winning for each strategy: (5/6 11/12)
var : ball1
w: 5/6 (0.8333333333333334)
b: 1/6 (0.16666666666666666)

var : win1
#t: 5/6 (0.8333333333333334)
#f: 1/6 (0.16666666666666666)
mean: 5/6 (0.8333333333333334)

var : ball2
w: 11/12 (0.9166666666666666)
b: 1/12 (0.08333333333333333)

var : win2
#t: 11/12 (0.9166666666666666)
#f: 1/12 (0.08333333333333333)
mean: 11/12 (0.9166666666666666)

var : s2-beats-s1
#f: 61/72 (0.8472222222222222)
#t: 11/72 (0.1527777777777778)
mean: 11/72 (0.1527777777777778)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;; The theoretical probabilities for the two strategies
(define (theoreticalProb a b)
  (let ([strat1 (/ a (+ a b))]
        [strat2 (* (/ a (+ a b)) (+ 1 (/ 1 (+ a b -2))))])
    (list strat1 strat2)))

(define (model a b)
  (displayln (format "\n* a: ~a b: ~a" a b))
  (displayln (format "Theoretical probability of winning for each strategy: ~a" (theoreticalProb a b)))
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define colors (vector "w" "b"))
   
   (defmem (strategy1)
     (let ([balls (append (ones-list a "w") (ones-list b "b"))])
       ;; A draws a ball: if white then win, else loss
       (uniform-draw balls)))
   
   (define (strategy2 numWhite numBlack)
     (let* ([total (+ numWhite numBlack)]
            ;; A draws one ball and remove it
            [ball1 (categorical-vw2 (vector (/ numWhite total) (/ numBlack total)) colors)]
            [numWhite2 (if (eq? ball1 "w") (sub1 numWhite) numWhite)]
            [numBlack2 (if (eq? ball1 "b") (sub1 numBlack) numBlack)]
            
            ;; B remove one black ball
            [numWhite3 numWhite2]
            [numBlack3 (sub1 numBlack2)]
            [total3 (- total 2)]; ;; we have now removed 2 balls.            
            ;; A draw again. If white: win else loss            
            [ball (categorical-vw2 (vector (/ numWhite3 total3) (/ numBlack3 total3)) colors)])
            ball
            ))
   
   (define ball1 (strategy1));
   (define win1 (eq? ball1 "w"))  
   (define ball2 (strategy2 a b))
   (define win2 (eq? ball2 "w"))
   (define s2-beats-s1 (> (boolean->integer win2) (boolean->integer win1)))
     
   (list ball1
         win1
         ball2
         win2
         s2-beats-s1
         )
   
   )
)

(for ([ab '( (1 2)
             (1 3)
             (1 10)
             (2 10)
             (2 2)
             (10 10)
             (10 2))])
  (show-marginals (apply model ab)
                  (list  "ball1"
                         "win1"
                         "ball2"
                         "win2"
                         "s2-beats-s1"
                         )
                  #:num-samples 1000
                  ; #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ))

