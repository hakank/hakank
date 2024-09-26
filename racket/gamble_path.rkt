#| 

  Path in Racket.Gamble 

  From "Probabilistic Logic Programming Under the Distribution Semantics"
  page 11
  """
  An interesting application of PLP under the distribution semantics
  is the computation of the probability of a path between two nodes in a graph in
  which the presence of each edge is probabilistic:

  This program, coded in ProbLog, was used in (10) for computing the probability
  that two biological concepts are related in the BIOMINE network (26).
  """

  * Enumerate #:limit 1e-02

path2 c c: #t
var : path1 a
c: 0.6015959619067899
b: 0.29960419135981936
-: 0.0987998467333913

var : path1 b
-: 0.8016989810563933
c: 0.1983010189436076

var : path1 c
-: 0.901977745735134
a: 0.0980222542648667

var : path2 a a
#t: 1.0000000000000009
mean: 1.0000000000000009

var : path2 a b
#f: 0.6997403084007109
#t: 0.30025969159929056
mean: 0.30025969159929056

var : path2 a c
#t: 0.6264538814806961
#f: 0.3735461185193046
mean: 0.6264538814806961

var : path2 b a
#f: 0.9801953031100386
#t: 0.01980469688996238
mean: 0.01980469688996238

var : path2 b b
#t: 1.0000000000000009
mean: 1.0000000000000009

var : path2 b c
#f: 0.7899326665876428
#t: 0.2100673334123578
mean: 0.2100673334123578

var : path2 c a
#f: 0.901274300463314
#t: 0.09872569953668611
mean: 0.09872569953668611

var : path2 c b
#f: 0.9707719963714687
#t: 0.029228003628532367
mean: 0.029228003628532367

var : path2 c c
#t: 1.0000000000000009
mean: 1.0000000000000009


  * Importance sampler 100000 samples

var : path1 a
c: 0.6000199999999998
b: 0.29935000000000056
-: 0.10063000000000004

var : path1 b
-: 0.8000799999999999
c: 0.1999200000000001

var : path1 c
-: 0.8981900000000003
a: 0.10180999999999998

var : path2 a a
#t: 1.0
mean: 1.0

var : path2 a b
#f: 0.6983199999999998
#t: 0.30168000000000017
mean: 0.30168000000000017

var : path2 a c
#t: 0.6258100000000004
#f: 0.3741900000000001
mean: 0.6258100000000004

var : path2 b a
#f: 0.9794100000000002
#t: 0.020589999999999966
mean: 0.020589999999999966

var : path2 b b
#t: 1.0
mean: 1.0

var : path2 b c
#f: 0.7898900000000003
#t: 0.21011000000000013
mean: 0.21011000000000013

var : path2 c a
#f: 0.8991900000000005
#t: 0.10081000000000004
mean: 0.10081000000000004

var : path2 c b
#f: 0.9697500000000004
#t: 0.030249999999999975
mean: 0.030249999999999975

var : path2 c c
#t: 1.0
mean: 1.0


  This is a port of my WebPPL model path.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate #:limit 1e-02
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define nodes '("a" "b" "c"))

   ;; Visit a specific node from another node.
   ;; We have to add an dummy node '""' to get the probabilities correct.
   (define (path1 node) 
     (cond
         [(eq? node "a") (categorical-vw2 (vector 0.3 0.6 0.1) (vector "b" "c" "-"))]
         [(eq? node "b") (categorical-vw2 (vector 0.2 0.8) (vector "c" "-"))]
         [(eq? node "c") (categorical-vw2 (vector 0.1 0.9) (vector "a" "-"))]))
        
   (define (path2 x y)
     (if (or (eq? x y)
             (eq? y (path1 x))
             ; sum(map(function(z) ( z != x && z != y && path1(z) == y && path2(x,z) ),nodes)) > 0
             (> (for/sum ([z nodes])
                  (boolean->integer (and (not (eq? z x))
                       (not (eq? z y))
                       (eq? y (path1 z))
                       (path2 x z)))) 0)
                )
         #t
         #f))

   
   (list (path1 "a")
         (path1 "b")
         (path1 "c")
         (path2 "a" "a")
         (path2 "a" "b")
         (path2 "a" "c")
         (path2 "b" "a")
         (path2 "b" "b")
         (path2 "b" "c")
         (path2 "c" "a")
         (path2 "c" "b")
         (path2 "c" "c")         
         )

   )
)

(show-marginals (model)
                (list  "path1 a"
                       "path1 b"
                       "path1 c"
                       "path2 a a"
                       "path2 a b"
                       "path2 a c"
                       "path2 b a"
                       "path2 b b"
                       "path2 b c"
                       "path2 c a"
                       "path2 c b"
                       "path2 c c"                       
                     )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


