#| 

  Ruin problem in Racket Gamble.

  This version use a fixed max time (length) of the sequences.

  For maxT=12 (enumerate)

var : len
1: 1/2 (0.5)
12: 231/1024 (0.2255859375)
3: 1/8 (0.125)
5: 1/16 (0.0625)
7: 5/128 (0.0390625)
9: 7/256 (0.02734375)
11: 21/1024 (0.0205078125)
mean: 4751/1024 (4.6396484375)

var : ruinIx
#f: 1/2 (0.5)
11: 231/1024 (0.2255859375)
2: 1/8 (0.125)
4: 1/16 (0.0625)
6: 5/128 (0.0390625)
8: 7/256 (0.02734375)
10: 21/1024 (0.0205078125)
mean: 3727/1024 (3.6396484375)

var : amount at maxT-1
0: 793/1024 (0.7744140625)
4: 165/2048 (0.08056640625)
2: 33/512 (0.064453125)
6: 55/1024 (0.0537109375)
8: 11/512 (0.021484375)
10: 5/1024 (0.0048828125)
12: 1/2048 (0.00048828125)
mean: 1 (1.0)

var : ruin at maxT-1
#t: 793/1024 (0.7744140625)
#f: 231/1024 (0.2255859375)
mean: 793/1024 (0.7744140625)

var : amount 0
1: 1 (1.0)
mean: 1 (1.0)

var : amount 1
0: 1/2 (0.5)
2: 1/2 (0.5)
mean: 1 (1.0)

var : amount 2
0: 1/2 (0.5)
1: 1/4 (0.25)
3: 1/4 (0.25)
mean: 1 (1.0)

var : amount 3
0: 5/8 (0.625)
2: 1/4 (0.25)
4: 1/8 (0.125)
mean: 1 (1.0)

var : amount 4
0: 5/8 (0.625)
3: 3/16 (0.1875)
1: 1/8 (0.125)
5: 1/16 (0.0625)
mean: 1 (1.0)

var : amount 5
0: 11/16 (0.6875)
2: 5/32 (0.15625)
4: 1/8 (0.125)
6: 1/32 (0.03125)
mean: 1 (1.0)

var : amount 6
0: 11/16 (0.6875)
3: 9/64 (0.140625)
1: 5/64 (0.078125)
5: 5/64 (0.078125)
7: 1/64 (0.015625)
mean: 1 (1.0)

var : amount 7
0: 93/128 (0.7265625)
2: 7/64 (0.109375)
4: 7/64 (0.109375)
6: 3/64 (0.046875)
8: 1/128 (0.0078125)
mean: 1 (1.0)

var : amount 8
0: 93/128 (0.7265625)
3: 7/64 (0.109375)
5: 5/64 (0.078125)
1: 7/128 (0.0546875)
7: 7/256 (0.02734375)
9: 1/256 (0.00390625)
mean: 1 (1.0)

var : amount 9
0: 193/256 (0.75390625)
4: 3/32 (0.09375)
2: 21/256 (0.08203125)
6: 27/512 (0.052734375)
8: 1/64 (0.015625)
10: 1/512 (0.001953125)
mean: 1 (1.0)

var : amount 10
0: 193/256 (0.75390625)
3: 45/512 (0.087890625)
5: 75/1024 (0.0732421875)
1: 21/512 (0.041015625)
7: 35/1024 (0.0341796875)
9: 9/1024 (0.0087890625)
11: 1/1024 (0.0009765625)
mean: 1 (1.0)

var : amount 11
0: 793/1024 (0.7744140625)
4: 165/2048 (0.08056640625)
2: 33/512 (0.064453125)
6: 55/1024 (0.0537109375)
8: 11/512 (0.021484375)
10: 5/1024 (0.0048828125)
12: 1/2048 (0.00048828125)
mean: 1 (1.0)

var : ruin 0
#f: 1 (1.0)
mean: 0 (0.0)

var : ruin 1
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)

var : ruin 2
#f: 1/2 (0.5)
#t: 1/2 (0.5)
mean: 1/2 (0.5)

var : ruin 3
#t: 5/8 (0.625)
#f: 3/8 (0.375)
mean: 5/8 (0.625)

var : ruin 4
#t: 5/8 (0.625)
#f: 3/8 (0.375)
mean: 5/8 (0.625)

var : ruin 5
#t: 11/16 (0.6875)
#f: 5/16 (0.3125)
mean: 11/16 (0.6875)

var : ruin 6
#t: 11/16 (0.6875)
#f: 5/16 (0.3125)
mean: 11/16 (0.6875)

var : ruin 7
#t: 93/128 (0.7265625)
#f: 35/128 (0.2734375)
mean: 93/128 (0.7265625)

var : ruin 8
#t: 93/128 (0.7265625)
#f: 35/128 (0.2734375)
mean: 93/128 (0.7265625)

var : ruin 9
#t: 193/256 (0.75390625)
#f: 63/256 (0.24609375)
mean: 193/256 (0.75390625)

var : ruin 10
#t: 193/256 (0.75390625)
#f: 63/256 (0.24609375)
mean: 193/256 (0.75390625)

var : ruin 11
#t: 793/1024 (0.7744140625)
#f: 231/1024 (0.2255859375)
mean: 793/1024 (0.7744140625)


  This is a port of my WebPPL model ruin_problem.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  
  (enumerate 
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define maxT 12)
    
   (define coins '("head" "tail"))
   (define start 1)
   (define win   1)
   (define loose 1)
    
   ;; Draw a coin at timestep t:
   (define draw (mem (lambda (t) 
                       (if (= t 0) 
                           "head"
                           (uniform-draw coins)))))

    ;; What is the score at time t?
    ;; After ruin there's no way back at the game again.
   (define (amount1 t)
     (cond
       [(= t 0) start]
       [(<= (amount1 (- t 1)) 0) 0]
       [(eq? (draw t) "head") (+ (amount1 (- t 1)) win)]
       [else 
            (if (< (- (amount1 (- t 1)) loose) 0)
                0
                (- (amount1 (- t 1)) loose))]))
   
   (define amount (mem (lambda (t)
                         (amount1 t))))
   
   ;; Is the player busted at time t?
   ;; This is the first time the player bused
   (define ruin (mem (lambda (t) 
                       (if (<= (amount t) 0) 
                           #t
                           #f))))
     
   (define amountArray (for/list ([t maxT]
                                  #:do [(define a (amount t))]
                                  #:when (> a 0)
                                  )
                         a))

   (define len (length amountArray))

   (define ruinIx (for/last ([t (range 1 maxT)]
                               #:break (ruin t))
                     t))
    
   ; (observe/fail (ruin 5))
   ; (observe/fail (= (amount (- maxT 1)) 8))
    
   (list 
        ;; amount: amountArray,
        len
        ruinIx 
        (amount (- maxT 1))
        (ruin   (- maxT 1))
        
        (amount 0)
        (amount 1)
        (amount 2)
        (amount 3)
        (amount 4)
        (amount 5)
        (amount 6)
        (amount 7)
        (amount 8)
        (amount 9)
        (amount 10)
        (amount 11)
        
        (ruin 0)
        (ruin 1)
        (ruin 2)
        (ruin 3)
        (ruin 4)
        (ruin 5)
        (ruin 6)
        (ruin 7)
        (ruin 8)
        (ruin 9)
        (ruin 10)
        (ruin 11)

        )
  
   )
  )

(show-marginals (model)
                (list "len"
                      "ruinIx"
                      "amount at maxT-1"
                      "ruin at maxT-1"
                      "amount 0"
                      "amount 1"
                      "amount 2"
                      "amount 3"
                      "amount 4"
                      "amount 5"
                      "amount 6"
                      "amount 7"
                      "amount 8"
                      "amount 9"
                      "amount 10"
                      "amount 11"
                      "ruin 0"
                      "ruin 1"
                      "ruin 2"
                      "ruin 3"
                      "ruin 4"
                      "ruin 5"
                      "ruin 6"
                      "ruin 7"
                      "ruin 8"
                      "ruin 9"
                      "ruin 10"
                      "ruin 11"
                      
                      
                       )
                #:num-samples 10000
                )

