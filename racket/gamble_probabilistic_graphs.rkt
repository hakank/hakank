#| 

  Probabilstic graphs in Racket.Gamble 

  https://dtai.cs.kuleuven.be/problog/tutorial/basic/04_pgraph.html
  ProbLog model:
  """
  0.6::edge(1,2).
  0.1::edge(1,3).
  0.4::edge(2,5).
  0.3::edge(2,6).
  0.3::edge(3,4).
  0.8::edge(4,5).
  0.2::edge(5,6).

  path(X,Y) :- edge(X,Y).
  path(X,Y) :- edge(X,Z),
               Y \== Z,
               path(Z,Y).

  query(path(1,5)).
  query(path(1,6)).
  query(path(5,2)).
  """
 
  Results from the ProbLog model
  """
  path(1,5): 0.25824    (i.e. 0->4)
  path(1,6): 0.2167296  (i.e. 0->5)
  """

  Using enumerate #:limit 1e-10:

  var : path 0 4 (1 5)
  #f: 0.74176
  #t: 0.25823999999999997
  mean: 0.25823999999999997

  var : path 0 5 (1 5)
  #f: 0.7832704
  #t: 0.21672959999999997
  mean: 0.21672959999999997


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-10
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (define edges (range 6))

   ; Graph
   ;  0 -> 1,2
   ;  1 -> 4,5
   ;  2 -> 3
   ;  3 -> 4
   ;  4 -> 5
   ;  5 -> ()   
   (define m '(
               ;; 0    1    2    3    4    5 
               (0.0  0.6  0.1  0.0  0.0  0.0) ; 0
               (0.0  0.0  0.0  0.0  0.4  0.3) ; 1
               (0.0  0.0  0.0  0.3  0.0  0.0) ; 2
               (0.0  0.0  0.0  0.0  0.8  0.0) ; 3
               (0.0  0.0  0.0  0.0  0.0  0.2) ; 4
               (0.0  0.0  0.0  0.0  0.0  0.0) ; 5
               ))
   
   (defmem (edge t1 t2) (flip (list-ref2d m t1 t2)))
   (defmem (path t1 t2)
     (if (edge t1 t2)
         #t
         (if (ormap (lambda (t3)
                      (and (not (= t2 t3))
                           (not (= t1 t3))
                           (edge t1 t3)
                           (path t3 t2)))
                      edges)
             #t
             #f
             )))

   (list (path 0 4) ; 1->5 in the ProbLog model
         (path 0 5) ; 1->6 in the ProbLog model
         )
   
   )
)

(show-marginals (model)
                (list  "path 0 4 (1 5)"
                       "path 0 5 (1 5)"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


