;;;
;;;
;;; Problem from Graphplan's tsp_peterson_facts.htm
;;; """
;;; This is the Peterson graph.  10 nodes, but best path back to start has
;;; length 11.
;;; """
;;;
;;; hakank: There is no solution when the

(define (problem tsp-03)
   (:domain tsp)
   (:objects A B C D E F G H I J - node)
   (:init

    ;;; Directed graph
    (connected A B)
    (connected B A)
    (connected A C)
    (connected C A)
    (connected A I)
    (connected I A)
    (connected B F)
    (connected F B)
    (connected B H)
    (connected H B)
    (connected C D)
    (connected D C)
    (connected C E)
    (connected E C)
    (connected D H)
    (connected H D)
    (connected D J)
    (connected J D)
    (connected E F)
    (connected F E)
    (connected E G)
    (connected G E)
    (connected F J)
    (connected J F)
    (connected G H)
    (connected H G)
    (connected G I)
    (connected I G)
    (connected I J)
    (connected J I)

    ;;; start
    (at A)
   )

   (:goal 
       (and 
        (at A)
        (forall (?node - node) (visited ?node))
        )

       )
)
