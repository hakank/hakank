;;;
;;; Shortest path problem in PDDL.
;;;
;;; Inspired by the Graphplan problem shortestpath_ops.htm / shortestpath_facts.htm
;;;

 ;; MOVE
 ;; (params (<start> <end> CONNECTED))
 ;; (preconds  (at <start>))
 ;; (effects   (at <end>) (del at <start>)))

(define (domain shortest)
   (:requirements :strips)
   (:predicates 
                 (move ?from ?to) 
                 (at ?pos)
                 (connected ?from ?to)
                 )

   (:action move
             :parameters (?from ?to)
             :precondition (and 
                             (at ?from) 
                             ;;; (connected ?from ?to) 
                             (or
                              (connected ?from ?to)
                              (connected ?to ?from))
                             )
             :effect  (and 
                           (at ?to)
                           (not (at ?from)))
    ) 


)

