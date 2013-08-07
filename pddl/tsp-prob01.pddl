;;;
;;;
;;; Problem from Graphplan's tsp_fact
;;; """
;;; In this one, all roads are two way except the road from Toronto to
;;; Pittsburgh is one-way.
;;; """
;;;

(define (problem tsp-01)
   (:domain tsp)
   (:objects Boston NewYork Boston Pittsburgh Toronto Albany - node)
   (:init

    ;;; Note: this is a directed graph
    (connected Boston NewYork)
    (connected NewYork Boston)
    (connected Pittsburgh Boston)
    (connected Boston Pittsburgh)
    (connected Pittsburgh NewYork)
    (connected NewYork Pittsburgh)
    (connected Toronto Pittsburgh)
    (connected Toronto NewYork)
    (connected NewYork Toronto)
    (connected NewYork Albany)
    (connected Albany NewYork)
    (connected Albany Toronto)
    (connected Toronto Albany)
    
    ;;; start
    (at Pittsburgh)
   )

   (:goal 
       (and 
        (at Pittsburgh)

        ;; (visited Boston)
        ;; (visited NewYork)
        ;; (visited Pittsburgh)
        ;; (visited Toronto)
        ;; (visited Albany)

        ;;; This is nicer, works with FF, etc
        (forall (?node - node) (visited ?node))

        )

       )
)
