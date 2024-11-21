#| 

  Arithmetic two-step in Racket/Gamble 

  https://medium.com/@shelvia1039/new-scientists-brain-twister-18-the-arithmetical-two-step-a12f49467615
  """
  This year, New Scientist has embarked on a new series for their puzzle column called 
  the "Brain Twister". I believe it will be a fun exercise for me to tackle these puzzles 
  using Python, as I aim to improve my coding skills. The problem that we’re gonna solve 
  in this post is "The Arithmetical Two-Step".

  Problem:
  
  You must take two steps to get from a given number to make 10. Each step must change 
  the number by adding, subtracting, multiplying by or dividing by a number from 1 to 9. 
  (Multiplying or dividing by 1 isn’t allowed as it doesn’t change the number).

  For example, starting with 35, one way would be to first divide by 7 then add 5.

  Questions:

  (1) Can you take two steps to get from 42 to 10?

  (2) Is it possible to get to 10 in two steps from all the numbers 11 to 30?

  (3) And can you find a two-digit number for which there is no way to get to 10 in two steps?
  """

  Using probabilistic programming for this problem is probably not the best approach;  
  a better approach would be constraint programming or planning.
  That being said, enumerate actually works quite nicely...

  So here we go.

  * 42 -> 10
(2 ((42) (42 - 2 = 40) (40 / 4 = 10))): 1/5 (0.2)
(2 ((42) (42 + 8 = 50) (50 / 5 = 10))): 1/5 (0.2)
(2 ((42) (42 / 3 = 14) (14 - 4 = 10))): 1/5 (0.2)
(2 ((42) (42 / 7 = 6) (6 + 4 = 10))): 1/5 (0.2)
(2 ((42) (42 / 6 = 7) (7 + 3 = 10))): 1/5 (0.2)

  * From 42 to 11..30
num-steps: 2 from: 42 -> 10
variable : res
(2 ((42) (42 - 2 = 40) (40 / 4 = 10))): 1/5 (0.2)
(2 ((42) (42 + 8 = 50) (50 / 5 = 10))): 1/5 (0.2)
(2 ((42) (42 / 3 = 14) (14 - 4 = 10))): 1/5 (0.2)
(2 ((42) (42 / 7 = 6) (6 + 4 = 10))): 1/5 (0.2)
(2 ((42) (42 / 6 = 7) (7 + 3 = 10))): 1/5 (0.2)


num-steps: 2 from: 42 -> 11
variable : res
(2 ((42) (42 / 3 = 14) (14 - 3 = 11))): 1/5 (0.2)
(2 ((42) (42 / 7 = 6) (6 + 5 = 11))): 1/5 (0.2)
(2 ((42) (42 - 9 = 33) (33 / 3 = 11))): 1/5 (0.2)
(2 ((42) (42 / 6 = 7) (7 + 4 = 11))): 1/5 (0.2)
(2 ((42) (42 + 2 = 44) (44 / 4 = 11))): 1/5 (0.2)


num-steps: 2 from: 42 -> 12
variable : res
(2 ((42) (42 / 2 = 21) (21 - 9 = 12))): 1/8 (0.125)
(2 ((42) (42 * 2 = 84) (84 / 7 = 12))): 1/8 (0.125)
(2 ((42) (42 / 7 = 6) (6 * 2 = 12))): 1/8 (0.125)
(2 ((42) (42 / 6 = 7) (7 + 5 = 12))): 1/8 (0.125)
(2 ((42) (42 - 6 = 36) (36 / 3 = 12))): 1/8 (0.125)
(2 ((42) (42 / 3 = 14) (14 - 2 = 12))): 1/8 (0.125)
(2 ((42) (42 + 6 = 48) (48 / 4 = 12))): 1/8 (0.125)
(2 ((42) (42 / 7 = 6) (6 + 6 = 12))): 1/8 (0.125)


num-steps: 2 from: 42 -> 13
variable : res
(2 ((42) (42 / 7 = 6) (6 + 7 = 13))): 1/5 (0.2)
(2 ((42) (42 / 3 = 14) (14 - 1 = 13))): 1/5 (0.2)
(2 ((42) (42 / 6 = 7) (7 + 6 = 13))): 1/5 (0.2)
(2 ((42) (42 - 3 = 39) (39 / 3 = 13))): 1/5 (0.2)
(2 ((42) (42 / 2 = 21) (21 - 8 = 13))): 1/5 (0.2)


num-steps: 2 from: 42 -> 14
variable : res
(1 ((42) (42 / 3 = 14))): 4/5 (0.8)
(2 ((42) (42 / 9 = 14/3) (14/3 * 3 = 14))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 1 = 42) (42 / 3 = 14))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 2 = 21) (21 - 7 = 14))): 1/45 (0.022222222222222223)
(2 ((42) (42 * 3 = 126) (126 / 9 = 14))): 1/45 (0.022222222222222223)
(2 ((42) (42 * 2 = 84) (84 / 6 = 14))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 6 = 7) (7 + 7 = 14))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 7 = 6) (6 + 8 = 14))): 1/45 (0.022222222222222223)
(2 ((42) (42 * 1 = 42) (42 / 3 = 14))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 6 = 7) (7 * 2 = 14))): 1/45 (0.022222222222222223)


num-steps: 2 from: 42 -> 15
variable : res
(2 ((42) (42 + 3 = 45) (45 / 3 = 15))): 1/5 (0.2)
(2 ((42) (42 / 6 = 7) (7 + 8 = 15))): 1/5 (0.2)
(2 ((42) (42 / 3 = 14) (14 + 1 = 15))): 1/5 (0.2)
(2 ((42) (42 / 7 = 6) (6 + 9 = 15))): 1/5 (0.2)
(2 ((42) (42 / 2 = 21) (21 - 6 = 15))): 1/5 (0.2)


num-steps: 2 from: 42 -> 16
variable : res
(2 ((42) (42 / 3 = 14) (14 + 2 = 16))): 1/4 (0.25)
(2 ((42) (42 / 6 = 7) (7 + 9 = 16))): 1/4 (0.25)
(2 ((42) (42 / 2 = 21) (21 - 5 = 16))): 1/4 (0.25)
(2 ((42) (42 + 6 = 48) (48 / 3 = 16))): 1/4 (0.25)


num-steps: 2 from: 42 -> 17
variable : res
(2 ((42) (42 / 2 = 21) (21 - 4 = 17))): 1/4 (0.25)
(2 ((42) (42 - 8 = 34) (34 / 2 = 17))): 1/4 (0.25)
(2 ((42) (42 + 9 = 51) (51 / 3 = 17))): 1/4 (0.25)
(2 ((42) (42 / 3 = 14) (14 + 3 = 17))): 1/4 (0.25)


num-steps: 2 from: 42 -> 18
variable : res
(2 ((42) (42 / 2 = 21) (21 - 3 = 18))): 1/5 (0.2)
(2 ((42) (42 - 6 = 36) (36 / 2 = 18))): 1/5 (0.2)
(2 ((42) (42 * 3 = 126) (126 / 7 = 18))): 1/5 (0.2)
(2 ((42) (42 / 3 = 14) (14 + 4 = 18))): 1/5 (0.2)
(2 ((42) (42 / 7 = 6) (6 * 3 = 18))): 1/5 (0.2)


num-steps: 2 from: 42 -> 19
variable : res
(2 ((42) (42 / 2 = 21) (21 - 2 = 19))): 1/3 (0.3333333333333333)
(2 ((42) (42 / 3 = 14) (14 + 5 = 19))): 1/3 (0.3333333333333333)
(2 ((42) (42 - 4 = 38) (38 / 2 = 19))): 1/3 (0.3333333333333333)


num-steps: 2 from: 42 -> 20
variable : res
(2 ((42) (42 - 2 = 40) (40 / 2 = 20))): 1/3 (0.3333333333333333)
(2 ((42) (42 / 2 = 21) (21 - 1 = 20))): 1/3 (0.3333333333333333)
(2 ((42) (42 / 3 = 14) (14 + 6 = 20))): 1/3 (0.3333333333333333)


num-steps: 2 from: 42 -> 21
variable : res
(1 ((42) (42 / 2 = 21))): 4/5 (0.8)
(2 ((42) (42 * 2 = 84) (84 / 4 = 21))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 8 = 21/4) (21/4 * 4 = 21))): 1/45 (0.022222222222222223)
(2 ((42) (42 * 1 = 42) (42 / 2 = 21))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 1 = 42) (42 / 2 = 21))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 6 = 7) (7 * 3 = 21))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 3 = 14) (14 + 7 = 21))): 1/45 (0.022222222222222223)
(2 ((42) (42 * 3 = 126) (126 / 6 = 21))): 1/45 (0.022222222222222223)
(2 ((42) (42 * 4 = 168) (168 / 8 = 21))): 1/45 (0.022222222222222223)
(2 ((42) (42 / 4 = 21/2) (21/2 * 2 = 21))): 1/45 (0.022222222222222223)


num-steps: 2 from: 42 -> 22
variable : res
(2 ((42) (42 / 3 = 14) (14 + 8 = 22))): 1/3 (0.3333333333333333)
(2 ((42) (42 / 2 = 21) (21 + 1 = 22))): 1/3 (0.3333333333333333)
(2 ((42) (42 + 2 = 44) (44 / 2 = 22))): 1/3 (0.3333333333333333)


num-steps: 2 from: 42 -> 23
variable : res
(2 ((42) (42 / 3 = 14) (14 + 9 = 23))): 1/3 (0.3333333333333333)
(2 ((42) (42 / 2 = 21) (21 + 2 = 23))): 1/3 (0.3333333333333333)
(2 ((42) (42 + 4 = 46) (46 / 2 = 23))): 1/3 (0.3333333333333333)


num-steps: 2 from: 42 -> 24
variable : res
(2 ((42) (42 / 7 = 6) (6 * 4 = 24))): 1/5 (0.2)
(2 ((42) (42 - 9 = 33) (33 - 9 = 24))): 1/5 (0.2)
(2 ((42) (42 + 6 = 48) (48 / 2 = 24))): 1/5 (0.2)
(2 ((42) (42 / 2 = 21) (21 + 3 = 24))): 1/5 (0.2)
(2 ((42) (42 * 4 = 168) (168 / 7 = 24))): 1/5 (0.2)


num-steps: 2 from: 42 -> 25
variable : res
(2 ((42) (42 / 2 = 21) (21 + 4 = 25))): 1/4 (0.25)
(2 ((42) (42 + 8 = 50) (50 / 2 = 25))): 1/4 (0.25)
(2 ((42) (42 - 8 = 34) (34 - 9 = 25))): 1/4 (0.25)
(2 ((42) (42 - 9 = 33) (33 - 8 = 25))): 1/4 (0.25)


num-steps: 2 from: 42 -> 26
variable : res
(2 ((42) (42 / 2 = 21) (21 + 5 = 26))): 1/4 (0.25)
(2 ((42) (42 - 9 = 33) (33 - 7 = 26))): 1/4 (0.25)
(2 ((42) (42 - 8 = 34) (34 - 8 = 26))): 1/4 (0.25)
(2 ((42) (42 - 7 = 35) (35 - 9 = 26))): 1/4 (0.25)


num-steps: 2 from: 42 -> 27
variable : res
(2 ((42) (42 - 8 = 34) (34 - 7 = 27))): 1/5 (0.2)
(2 ((42) (42 - 6 = 36) (36 - 9 = 27))): 1/5 (0.2)
(2 ((42) (42 / 2 = 21) (21 + 6 = 27))): 1/5 (0.2)
(2 ((42) (42 - 9 = 33) (33 - 6 = 27))): 1/5 (0.2)
(2 ((42) (42 - 7 = 35) (35 - 8 = 27))): 1/5 (0.2)


num-steps: 2 from: 42 -> 28
variable : res
(2 ((42) (42 - 6 = 36) (36 - 8 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 / 6 = 7) (7 * 4 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 - 5 = 37) (37 - 9 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 / 9 = 14/3) (14/3 * 6 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 * 6 = 252) (252 / 9 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 - 8 = 34) (34 - 6 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 / 2 = 21) (21 + 7 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 * 4 = 168) (168 / 6 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 - 9 = 33) (33 - 5 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 / 3 = 14) (14 * 2 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 * 2 = 84) (84 / 3 = 28))): 1/12 (0.08333333333333333)
(2 ((42) (42 - 7 = 35) (35 - 7 = 28))): 1/12 (0.08333333333333333)


num-steps: 2 from: 42 -> 29
variable : res
(2 ((42) (42 - 9 = 33) (33 - 4 = 29))): 1/7 (0.14285714285714285)
(2 ((42) (42 - 6 = 36) (36 - 7 = 29))): 1/7 (0.14285714285714285)
(2 ((42) (42 - 8 = 34) (34 - 5 = 29))): 1/7 (0.14285714285714285)
(2 ((42) (42 - 5 = 37) (37 - 8 = 29))): 1/7 (0.14285714285714285)
(2 ((42) (42 / 2 = 21) (21 + 8 = 29))): 1/7 (0.14285714285714285)
(2 ((42) (42 - 7 = 35) (35 - 6 = 29))): 1/7 (0.14285714285714285)
(2 ((42) (42 - 4 = 38) (38 - 9 = 29))): 1/7 (0.14285714285714285)


num-steps: 2 from: 42 -> 30
variable : res
(2 ((42) (42 * 5 = 210) (210 / 7 = 30))): 1/10 (0.1)
(2 ((42) (42 - 8 = 34) (34 - 4 = 30))): 1/10 (0.1)
(2 ((42) (42 - 4 = 38) (38 - 8 = 30))): 1/10 (0.1)
(2 ((42) (42 / 2 = 21) (21 + 9 = 30))): 1/10 (0.1)
(2 ((42) (42 / 7 = 6) (6 * 5 = 30))): 1/10 (0.1)
(2 ((42) (42 - 3 = 39) (39 - 9 = 30))): 1/10 (0.1)
(2 ((42) (42 - 6 = 36) (36 - 6 = 30))): 1/10 (0.1)
(2 ((42) (42 - 7 = 35) (35 - 5 = 30))): 1/10 (0.1)
(2 ((42) (42 - 5 = 37) (37 - 7 = 30))): 1/10 (0.1)
(2 ((42) (42 - 9 = 33) (33 - 3 = 30))): 1/10 (0.1)


  3) unsolvable for 42 -> 10..99 in 2 steps
     The following are unsolvable (from 42) are:
     ((42 61) (42 62) (42 64) (42 65) (42 67) (42 69) (42 71) (42 73) (42 95) (42 97))

     Ah, but the question was 
     from 10..99 -> 10

     Then the only unsolvable is 
      (10 10) and (50 10)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model from to num-steps)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define ops (list + - * /))
   (define numbers (range 1 10))
   (define (show-eq op a b) (format "~a ~a ~a =" a (conv-op op) b))
   (define (conv-op op)
     (cond
       [(eq? op +) "+"]
       [(eq? op *) "*"]
       [(eq? op -) "-"]
       [(eq? op /) "/"]
       ))

   (define (f from to)
     (define (loop a)
       (if (or (= (last (last a)) to) (> (length a) num-steps))
           a
           (let* ([val (last (last a))]
                  [op (uniform-draw ops)]
                  [num (uniform-draw numbers)]
                  [res (op val num)])
             ; val and res should be different
             (observe/fail (not (= val res)))
             ; res should be an integer (not a rational number)
             (observe/fail (integer? res))
             (loop (append a (list (list (show-eq op val num) res)))))))
     (loop (list (list from)))
     )
     
   (define res (f from to))
   (define len (sub1 (length res)))
   (observe/fail (= len num-steps))
   (observe/fail (= (last (last res)) to))
   ; (show "res" res)
   (list ; res
         (list (sub1 (length res)) res)
         )

   )
)

(define unsolvable '())
(for ([num-steps (range 2 3)])
  (for* (; Part 1 and 2
         [from (list 42)]
         [to (range 10 31)]
         ; Part 3
         ;; [from (range 10 100)]
         ;; [to (list 10)]
         )
  (newline)
  (displayln (format "num-steps: ~a from: ~a -> ~a" num-steps from to))
  (with-handlers ([exn:fail? (lambda (exn)
                               (displayln "no solution")
                               (set! unsolvable (append unsolvable (list (list from to)) )))])
    (show-marginals (model from to num-steps)
                    (list  "res"
                           "num-steps, res"
                           )
                    #:num-samples 100
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                  ; #:thin 0
                    )
    )
  )

)

(newline)
(show "unsolvable" unsolvable)
