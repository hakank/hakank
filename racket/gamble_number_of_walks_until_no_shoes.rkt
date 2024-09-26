#| 

  Number of walks until no shoes in Racket.Gamble 

  From 
  Gunnar Blom, Lars Holst, Dennis Sandell:
  "Problems and Snapshots from the World of Probability"
  Page 8f, Problem 1.6 Number of walks until no shoes

  """
  A has a house with one front door and one back door. He places
  two pairs of walking shoes at each door. For each walk, he selects
  one door at random, puts on a pair of shoes, returns after a walk
  to a randomly chosen door, and takes off the shoes at the door.
  We want to determine the expected number of finished walks until
  A discovers that no shoes are available at the door he has selected
  for his next walk.
  """

  * 2 2

(n 2 i 2 theoretical 12)
var : a
(l l): 0.031710000000020166
(r r): 0.031630000000020114
(l l l): 0.015510000000009863
(r r r): 0.015330000000009749
(l l r): 0.008440000000005367
...
(r r r r l l r r r l r r r r r l r r l l l r l l l r r): 1.000000000000636e-5
(l r r r l l r r l r l r r l l l r): 1.000000000000636e-5
(r l r r r l r l r l l r l l r r l l l r r r l l l l r): 1.000000000000636e-5
(r r l r r r l r r r l r l l r r r l r r r l): 1.000000000000636e-5
(r l r l r l r l r l r l l r l r r): 1.000000000000636e-5

var : len
4: 0.07890000000005018
3: 0.07837000000004984
5: 0.07329000000004661
6: 0.06660000000004236
2: 0.06334000000004028
...
98: 1.000000000000636e-5
100: 1.000000000000636e-5
101: 1.000000000000636e-5
118: 1.000000000000636e-5
124: 1.000000000000636e-5
mean: 11.988860000007698

  * 1 1
(n 1 i 1 theoretical 4)
var : a
(l): 0.12464000000000952
(r): 0.12405000000000947
(r l): 0.06232000000000476
(l r): 0.06118000000000467
(r r): 0.031540000000002406
...
(r l l l l r l l l r r r l l r r r r r l l r): 1.0000000000000765e-5
(l l r r l l l r l r r): 1.0000000000000765e-5
(l r r r l l l r l l l l r): 1.0000000000000765e-5
(l l r l r l l l r l l r): 1.0000000000000765e-5
(l l l r r l l r l l r l r l): 1.0000000000000765e-5

var : len
1: 0.248690000000019
2: 0.18624000000001423
3: 0.1423800000000109
4: 0.10811000000000824
5: 0.07777000000000593
...
33: 1.0000000000000765e-5
35: 1.0000000000000765e-5
36: 1.0000000000000765e-5
40: 1.0000000000000765e-5
43: 1.0000000000000765e-5
mean: 3.995770000000303

  * 3 3

(n 3 i 3 theoretical 24)
var : a
(r r r): 0.007560000000009343
(l l l): 0.007540000000009319
(l l l l): 0.006270000000007749
(r r r r): 0.005730000000007082
(l l l l l): 0.0030600000000037815
...
(l l l r l l r r l l l r l l): 1.0000000000012359e-5
(r r r r l l l l r r l r l l r): 1.0000000000012359e-5
(l r l r l r r r l l l r l r l r l): 1.0000000000012359e-5
(r r r l l l r r r l r r l r r l r l l r r l r l r r): 1.0000000000012359e-5
(r l r r r l l l r l l r r l l l r l l r r l l r r r l l l r r r l l r r l r l): 1.0000000000012359e-5

var : len
7: 0.03984000000004925
8: 0.03901000000004817
6: 0.03878000000004793
9: 0.037630000000046446
10: 0.037570000000046386
...
206: 1.0000000000012359e-5
208: 1.0000000000012359e-5
209: 1.0000000000012359e-5
217: 1.0000000000012359e-5
236: 1.0000000000012359e-5
mean: 23.85566000002951



  This is a port of my WebPPL model number_of_walks_until_no_shoes.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

;; The theoretical probability
(define (theoreticalProb n i)
  (- (+ (* 2 n) (* 4 n i)) (* 2 i i))
)


(define (model n i)
  (show2 "n" n "i" i "theoretical" (theoreticalProb n i))
  
  (; enumerate ; #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; Take shoes from a random door
   ;; Note: we only add to a when taking the shoes.    
   (define (takeShoes a left right)
     ;; Pick a door and check if there are any shoes
     (define pick (if (flip 0.5) "l" "r"))
     (if (or
          (and (eq? pick "l") (= left 0)) 
          (and (eq? pick "r") (= right 0)))
         a
         (if (eq? pick "l")
             (leaveShoes (cons pick a) (sub1 left) right)
             (leaveShoes (cons pick a) left (sub1 right)))))

   ;; Leave shoes at a random door
   (define (leaveShoes a left right)
     (if (flip 0.5)
         (takeShoes a (add1 left) right)
         (takeShoes a left (add1 right))
         )
     )

   (define a (takeShoes '() n n))
                            
   (list a
         (length a))

   )
)

(define (run n i) 
  (show-marginals (model n i)
                  (list  "a"
                         "len"
                         )
                  #:num-samples 10000
                  #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  )
  )

(run 2 2)
(run 1 1)
(run 3 3)

