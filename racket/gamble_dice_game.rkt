#| 

  Dice game in Racket Gamble.

  From
  http://www.informs.org/ORMS-Today/Public-Articles/December-Volume-38-Number-6/THE-PUZZLOR
  """
     A         B           C            D
     7          4          6            5 
   1 1 1 1    4 4 4 4    2 2 2 2      3 5 3 3
     7          4          6            5
  Figure 1 shows four dice with varying numbers on each face. You and 
  three friends will play a simple game where you will each roll one 
  of the dice and the highest number wins. You get first pick from 
  the dice.

  Which die should you choose in order to maximize your chance of winning?
  """
 
  Analysis
    A vs B:  A wins 2 times, B wins 4 times
    A vs C:  A wins 2 times, C wins 4 times
    A vs D:  A wins 2 times, D wins 4 times
    B vs C:  B wins 4 times, C wins 2 times
    B vs D:  B wins 3 times, D wins 3 times
    C vs D:  C wins 2 times, D wins 4 times

    Wins (of row vs col)
      A  B  C  D    Sum (winning)
   A  -  2  2  2      6    0.166667
   B  4  -  4  3     11    0.305556   <---
   C  4  2  -  2      8    0.222222
   D  4  4  3  -     11    0.305556   <---
                    --- 
                     36



  var : A
  mean: 1.9975000000000014

  var : B
  mean: 3.664800000000004

  var : C
  mean: 2.6571000000000025

  var : D
  mean: 3.680600000000003

  var : Ap
  mean: 0.1664583333333335

  var : Bp
  mean: 0.3054000000000003   <--

  var : Cp
  mean: 0.2214250000000002

  var : Dp
  mean: 0.30671666666666697  <--



  The matrix

  Number of wins:
  0                   1.6820000000000013 2.306000000000002  2.974000000000002
  1.6830000000000012  0                  2.314000000000002  3.046000000000002
  2.3340000000000014  2.330000000000002  0                  3.6700000000000026
  2.932000000000002   3.042000000000002  3.6490000000000027 0

  As percentages:

  0                    0.14016666666666677  0.19750000000000015  0.24708333333333352
  0.1402500000000001   0                    0.19450000000000012  0.2536666666666668
  0.19350000000000014  0.19416666666666682  0                    0.3045000000000002
  0.2535833333333335   0.2563333333333335   0.30416666666666686  0


  This is a port of my Turing.jl model dice_game.jl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (model [only-matrix? #f])
 
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define S '((1 1 1 1 7 7)
               (4 4 4 4 4 4)
               (2 2 2 2 6 6)
               (3 5 3 3 5 5)
               ))
   (define n (length S))

   (define num-sides (length (first S)))
   
   (define throws (for/list ([i n])
                    (for/list ([j n])
                      (if (= i j)
                          0
                          ; Roll the two dice
                          (let ([iix (random-integer num-sides)]
                                [jix (random-integer num-sides)])
                            ; Save the winner
                            (if (> (list-ref2d S i iix) (list-ref2d S j jix))
                                (add1 i)
                                (add1 j)))))))
   ; (show "throws" throws)

   (define (check-wins d)
     (for/sum ([i n])
       (for/sum ([j n])
         (if (= d (list-ref2d throws i j)) 1 0 ))))
   
   ; Number of wins
   (define A (check-wins 1))
   (define B (check-wins 2))
   (define C (check-wins 3))
   (define D (check-wins 4))

   ; (show2 "throws" throws "A" A "B" B "C" C "D" D)
   
   ; Percentages
   (define total (+ A B C D))
   (define Ap (/ A total))
   (define Bp (/ B total))
   (define Cp (/ C total))
   (define Dp (/ D total))

   (if only-matrix?
       (list (/ (list-ref2d throws 0 0) total)
             (/ (list-ref2d throws 0 1) total)
             (/ (list-ref2d throws 0 2) total)
             (/ (list-ref2d throws 0 3) total)
             
             (/ (list-ref2d throws 1 0) total)
             (/ (list-ref2d throws 1 1) total)
             (/ (list-ref2d throws 1 2) total)
             (/ (list-ref2d throws 1 3) total)
             
             (/ (list-ref2d throws 2 0) total)
             (/ (list-ref2d throws 2 1) total)
             (/ (list-ref2d throws 2 2) total)
             (/ (list-ref2d throws 2 3) total)
             
             (/ (list-ref2d throws 3 0) total)
             (/ (list-ref2d throws 3 1) total)
             (/ (list-ref2d throws 3 2) total)
             (/ (list-ref2d throws 3 3) total)
             )
       (list A
             B
             C
             D
             Ap
             Bp
             Cp
             Dp
             )
       )
   
   
   )
  )

(show-marginals (model #f)
                (list "A"
                      "B"
                      "C"
                      "D"
                      
                      "Ap"
                      "Bp"
                      "Cp"
                      "Dp"
                      )
                #:num-samples 10000
                ; #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
)

; For the matrix
#|
(displayln "\nThe matrix")
(show-marginals (model #t)
                (list "A A"
                      "A B"
                      "A C"
                      "A D"
                      "B A"
                      "B B"
                      "B C"
                      "B D"
                      "C A"
                      "C B"
                      "C C"
                      "C D"
                      "D A"
                      "D B"
                      "D C"
                      "D D"
                 )
                ; #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
)
|#

