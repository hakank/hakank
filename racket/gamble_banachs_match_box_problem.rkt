#| 

   Banach's match box problem in Racket Gamble.

   From 
   Gunnar Blom, Lars Holst, Dennis Sandell:
   "Problems and Snapshots from the World of Probability"
   Page 9f, Problem 1.7 Number of walks until no shoes

   """
   A person has, in each of his two pockets, a box with n matches.
   Now and then he talkes a match from a randomly chosen box until
   he finds the selected box empty. Find the expectation of the 
   number, R, of remaining matches in the other box.
   """

n: 50
var : len
100: 0.0812
99: 0.0794
96: 0.0772
98: 0.0771
97: 0.076
95: 0.0677
93: 0.0658
94: 0.0655
92: 0.0568
91: 0.0551
90: 0.051
89: 0.0423
88: 0.0361
87: 0.0335
86: 0.026
85: 0.0254
84: 0.0194
83: 0.0145
82: 0.0121
81: 0.0104
80: 0.0074
79: 0.006
78: 0.0043
77: 0.003
76: 0.0022
75: 0.002
74: 0.001
73: 0.0008
69: 0.0002
70: 0.0002
71: 0.0002
72: 0.0002
mean: 92.91430000000003
Credible interval (0.84): 87..100

var : numLeftInOtherBox
0: 0.0812
1: 0.0794
4: 0.0772
2: 0.0771
3: 0.076
5: 0.0677
7: 0.0658
6: 0.0655
8: 0.0568
9: 0.0551
10: 0.051
11: 0.0423
12: 0.0361
13: 0.0335
14: 0.026
15: 0.0254
16: 0.0194
17: 0.0145
18: 0.0121
19: 0.0104
20: 0.0074
21: 0.006
22: 0.0043
23: 0.003
24: 0.0022
25: 0.002
26: 0.001
27: 0.0008
28: 0.0002
29: 0.0002
30: 0.0002
31: 0.0002
mean: 7.0857
Credible interval (0.84): 0..13

(Theoretical result (Sterling approx) for numLeftInOtherBox=10 50 100 1000:)
(n 10 theoretical: 2.7020575410170005)
(n 50 theoretical: 7.038686950088869)
(n 100 theoretical: 10.326105889721207)
(n 1000 theoretical: 34.69586325392657)

  This is a port of my WebPPL model banachs_match_box_problem.wppl 
 
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (theoreticalProb  n)
  ;; The Stirling approach (from the book page 11):
  ; 2* Math.sqrt(n/Math.PI) - 1 + 3/(4*Math.sqrt(n*Math.PI));
  (+ (- (* 2 (sqrt (/ n pi))) 1) (/ 3 (* 4 (sqrt (* n pi)))))
)



(define (model n)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice) 
   
   ;; Pick a match box from a pocket and check if there
   ;; are any matches left.       
   (define (selectMatchBox a left right)
     (let ((pick (if (flip) "l" "r")))
       (cond
         [(or (and (eq? pick "l") (eq? left 0))
              (and (eq? pick "r") (eq? right 0)))
          (list a (if (eq? pick "l") right left))]
         [else (if (eq? pick "l")
                   (selectMatchBox (cons pick a) (sub1 left) right)
                   (selectMatchBox (cons pick a) left (sub1 right)))])
       ))

   (define res (selectMatchBox '() n n))
   (define a (first res))
   (define numLeftInOtherBox (second res))
                            
   (list ; a
         (length a)
         numLeftInOtherBox
        )

   )
  )

(define (run n)
  (show "n" n)
  (show-marginals (model n)
                  (list ; "a"
                        "len"
                        "numLeftInOtherBox"
                        )
                  #:num-samples 10000
                  ; #:truncate-output 4
                  ; #:skip-marginals? #t
                  #:credible-interval 0.84
                  ; #:credible-interval2 0.84                  
                  ; #:show-stats? #t
                  ; #:show-histogram? #t
                  )
  )

(run 50)
; (run 100)

(show2 "Theoretical result (Sterling approx) for numLeftInOtherBox=10 50 100 1000:")
(for ([n '(10 50 100 1000)])
  (show2 "n" n "theoretical:" (theoreticalProb n)))
(newline)
