;;;
;;; TSP problem in PDDL.
;;;
;;; From Graphplan's tsp_ops
;;;
;;; Note: Here we handle just the connections, not trying to 
;;; minimize any costs.
;;;;
(define (domain tsp)
   (:requirements :adl)
   (:types node)
   (:predicates 
                 (move ?from ?to - node) 
                 (at ?pos - node)
                 (connected ?start ?end - node)
                 (visited ?end - node)
                 )

   (:action move
             :parameters (?start ?end - node)
             :precondition (and 
                             (at ?start)
                             ;;; This is a directed graph
                             (connected ?start ?end) 

                             ;;; hakank: this should probably be included
                             (not (visited ?end)) 

                             ;;; For undirected graphs:
                             ;;; (or
                             ;;;   (connected ?start ?end)
                             ;;;   (connected ?end ?start))
                             )
             :effect  (and 
                           (at ?end)
                           (visited ?end)
                           (not (at ?start)))
    ) 

)

