#| 

  Class attendance model in Racket Gamble.

  From https://www.cs.cornell.edu/courses/cs4110/2016fa/lectures/lecture33.html
  """
  As a running example, let's imagine that we're building a system to recommend research papers 
  to students based on the classes they take. To keep things simple, let's say there are only 
  two research topics in the world: programming languages and statistics/machine learning. 
  Every paper is either a PL paper, a statistics paper, or both. And we'll consider three 
  courses at Cornell: CS 4110 (programming languages), CS 4780 (machine learning), and a 
  fictional elective, “CS 4242,” on probabilistic programming.

  It's pretty easy to imagine that machine learning should work for this problem: 
  the mixture of areas revealed by your class schedule should say something about the papers 
  you want to read. The problem is that the exact relationship can be hard to reason about 
  directly. Clearly taking 4780 means you're more likely to be interested in statistics, 
  but exactly how much more likely? What if you registered for 4780 because it was the 
  only class that fit into your schedule? What do we do about people who only take the 
  fictional CS 4242 and neither real course—do we just assume they're 50/50 PL/stats people? 

  ... 

               Interrest     Busy?     Interest  
               in stats                in PL 


               Takes        Takes      Takes
               CS4780       CS4242     CS4110
  """

var : relevance
((paper1 #f) (paper2 #f) (paper3 #f)): 0.25
((paper1 #t) (paper2 #t) (paper3 #t)): 0.24999999999999992
((paper1 #f) (paper2 #f) (paper3 #t)): 0.24999999999999975
((paper1 #f) (paper2 #t) (paper3 #f)): 0.24999999999999975

var : attendance
((cs4110 #f) (cs4780 #f) (cs4242 #f)): 0.4083749999999997
((cs4110 #t) (cs4780 #f) (cs4242 #f)): 0.1641249999999999
((cs4110 #f) (cs4780 #t) (cs4242 #f)): 0.1641249999999999
((cs4110 #t) (cs4780 #t) (cs4242 #t)): 0.07037499999999997
((cs4110 #f) (cs4780 #f) (cs4242 #t)): 0.06287499999999993
((cs4110 #t) (cs4780 #t) (cs4242 #f)): 0.05087499999999996
...
((cs4110 #t) (cs4780 #f) (cs4242 #t)): 0.03962499999999998
((cs4110 #f) (cs4780 #t) (cs4242 #t)): 0.039624999999999966

var : relevance_all
(): 0.25
((paper1 #t) (paper2 #t) (paper3 #t)): 0.24999999999999992
((paper3 #t)): 0.24999999999999975
((paper2 #t)): 0.24999999999999975

var : attendance_all
(): 0.40837499999999965
((cs4110 #t)): 0.16412499999999997
((cs4780 #t)): 0.1641249999999999
((cs4110 #t) (cs4780 #t) (cs4242 #t)): 0.07037499999999994
((cs4242 #t)): 0.06287499999999997
((cs4110 #t) (cs4780 #t)): 0.050875000000000004
...
((cs4780 #t) (cs4242 #t)): 0.039625
((cs4110 #t) (cs4242 #t)): 0.039624999999999994

var : combined
(): 0.18224999999999988
((paper3 #t) (cs4780 #t)): 0.11137499999999992
((paper2 #t) (cs4110 #t)): 0.1113749999999999
((paper3 #t)): 0.09112499999999994
((paper2 #t)): 0.09112499999999993
...
((paper3 #t) (cs4110 #t) (cs4780 #t) (cs4242 #t)): 0.0013749999999999995
((paper2 #t) (cs4110 #t) (cs4780 #t) (cs4242 #t)): 0.0013749999999999986
((paper2 #t) (cs4780 #t) (cs4242 #t)): 0.0011249999999999993
((paper3 #t) (cs4110 #t) (cs4242 #t)): 0.001124999999999999
((cs4110 #t) (cs4780 #t) (cs4242 #t)): 0.00024999999999999984

  * Paper recommendations:
  """
  Let's use the same philosophy now to actually produce recommendations. It's simple: we just need to 
  condition on the class registration of the person we're interested in. Here's an example that describes 
  me: I attend my own class, CS 4110, and the fictional PPL class, CS 4242, but not the ML class, 4780.
  """
 
var : relevance
((paper1 #t) (paper2 #t) (paper3 #t)): 0.6025236593059934
((paper1 #f) (paper2 #t) (paper3 #f)): 0.3123028391167194
((paper1 #f) (paper2 #f) (paper3 #f)): 0.05678233438485819
((paper1 #f) (paper2 #f) (paper3 #t)): 0.028391167192428988

var : attendance
((cs4110 #t) (cs4780 #f) (cs4242 #f)): 0.21611671924290227
((cs4110 #t) (cs4780 #t) (cs4242 #t)): 0.21435015772870658
((cs4110 #f) (cs4780 #f) (cs4242 #f)): 0.21142271293375386
((cs4110 #t) (cs4780 #t) (cs4242 #f)): 0.08441955835962142
((cs4110 #t) (cs4780 #f) (cs4242 #t)): 0.08353627760252366
((cs4110 #f) (cs4780 #t) (cs4242 #t)): 0.06681703470031544
...
((cs4110 #f) (cs4780 #t) (cs4242 #f)): 0.06564353312302834
((cs4110 #f) (cs4780 #f) (cs4242 #t)): 0.05769400630914824

var : relevance_all
((paper1 #t) (paper2 #t) (paper3 #t)): 0.6025236593059934
((paper2 #t)): 0.3123028391167194
(): 0.05678233438485819
((paper3 #t)): 0.028391167192428988

var : attendance_all
((cs4110 #t)): 0.2161167192429022
((cs4110 #t) (cs4780 #t) (cs4242 #t)): 0.21435015772870658
(): 0.21142271293375395
((cs4110 #t) (cs4780 #t)): 0.08441955835962142
((cs4110 #t) (cs4242 #t)): 0.08353627760252365
((cs4780 #t) (cs4242 #t)): 0.06681703470031543
...
((cs4780 #t)): 0.06564353312302831
((cs4242 #t)): 0.05769400630914821

var : combined
((paper1 #t) (paper2 #t) (paper3 #t) (cs4110 #t) (cs4780 #t) (cs4242 #t)): 0.2121041009463722
((paper2 #t) (cs4110 #t)): 0.16787697160883294
((paper2 #t)): 0.08508832807570982
((paper1 #t) (paper2 #t) (paper3 #t)): 0.0713974763406939
((paper1 #t) (paper2 #t) (paper3 #t) (cs4780 #t) (cs4242 #t)): 0.06420504731861197
...
((cs4110 #t) (cs4242 #t)): 0.0005110410094637221
((cs4780 #t) (cs4242 #t)): 0.0005110410094637221
((paper3 #t) (cs4110 #t) (cs4242 #t)): 0.00016719242902208197
((paper3 #t) (cs4110 #t) (cs4780 #t) (cs4242 #t)): 0.00011671924290220808
((cs4110 #t) (cs4780 #t) (cs4242 #t)): 5.6782334384858066e-5





  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; Class attendance model.
(define (attendance i_pl i_stats busy)
  (define (attendance_ interest busy)
    (if interest
      (if busy (flip 0.3) (flip 0.8))
      (flip 0.1)
    )
  )
  (define a_4110 (attendance_ i_pl  busy))
  (define a_4780 (attendance_ i_stats  busy))
  (define a_4242 (attendance_ (and i_pl i_stats) busy))

  (list (list "cs4110" a_4110)
        (list "cs4780" a_4780)
        (list "cs4242" a_4242))
  )

;; Relevance of our three papers.
(define (relevance i_pl i_stats) 
  (define rel1 (and i_pl i_stats))
  (define rel2 i_pl)
  (define rel3 i_stats)

  (list (list "paper1" rel1)
        (list "paper2" rel2)
        [list "paper3" rel3])
)



; A Combined model
(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler ; #:transition (enumerative-gibbs)

   ;; Some even random priors for our "student profile."
   (define i_pl    (flip 0.5))
   (define i_stats (flip 0.5))
   (define busy    (flip 0.5))

   ;; hakank: testing
   ; (observe/fail i_pl)

   ; Paper recommendation
   #|
   (match (attendance i_pl i_stats busy)
     [(list (list cs_4110 b1) (list cs_4780 b2) (list cs_4242 b3))
     (observe/fail (and b1 (not b2) b3))
     ]
     )
   |#

   
   (define relevance_all (filter (lambda (v)
                                   (eq? (second v) #t ))
                                 (relevance i_pl i_stats)))
   (define attendance_all (filter (lambda (v) (eq? (second v) #t ))
                                  (attendance i_pl i_stats busy)))


   
   (list (relevance i_pl i_stats)
         (attendance i_pl i_stats busy)
         relevance_all
         attendance_all
         (append relevance_all attendance_all)
         )

   )
  )

(show-marginals (model)
                (list "relevance"
                      "attendance"
                      "relevance_all"
                      "attendance_all"
                      "combined"
                      )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t                
                )
