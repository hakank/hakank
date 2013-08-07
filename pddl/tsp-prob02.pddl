;;;
;;;
;;; Problem from Graphplan's tsp_complete_facts.htm
;;; """
;; TSP on a complete graph. Would be easy for any normal planner, but hard for 
;; graphplan because it doesn't realize you can't visit n nodes in fewer
;; than n steps. [even though it does know that it can only do one action in
;; each time step and each action only satsifies up to one of the
;; "visited" goals, this info isn't used in any useful way.] The "L" lowerbounding
;; option tells graphplan to explicitly use this kind of reasoning. Specifically,
;; if you have a "clique" of t goals [maximal clique found greedily] such that
;; no two in the clique can be solved in the same step, then reason that
;; you will need at least t steps.  This option make it run LOTS faster.
;;; """
;;;

(define (problem tsp-02)
   (:domain tsp)
   (:objects A B C D E F G H I - node)
   (:init

    ;;; Directed graph
    (connected A B)
    (connected A C)
    (connected A D)
    (connected A E)
    (connected A F)
    (connected A G)
    (connected A H)
    (connected A I)
    
    (connected B A)
    (connected B C)
    (connected B D)
    (connected B E)
    (connected B F)
    (connected B G)
    (connected B H)
    (connected B I)

    (connected C A)
    (connected C B)
    (connected C D)
    (connected C E)
    (connected C F)
    (connected C G)
    (connected C H)
    (connected C I)

    (connected D A)
    (connected D B)
    (connected D C)
    (connected D E)
    (connected D F)
    (connected D G)
    (connected D H)
    (connected D I)
    
    (connected E A)
    (connected E B)
    (connected E C)
    (connected E D)
    (connected E F)
    (connected E G)
    (connected E H)
    (connected E I)
    
    (connected F A)
    (connected F B)
    (connected F C)
    (connected F D)
    (connected F E)
    (connected F G)
    (connected F H)
    (connected F I)
    
    (connected G A)
    (connected G B)
    (connected G C)
    (connected G D)
    (connected G E)
    (connected G F)
    (connected G H)
    (connected G I)
    
    (connected H A)
    (connected H B)
    (connected H C)
    (connected H D)
    (connected H E)
    (connected H F)
    (connected H G)
    (connected H I)
    
    (connected I A)
    (connected I B)
    (connected I C)
    (connected I D)
    (connected I E)
    (connected I F)
    (connected I G)
    (connected I H)
   
    ;;; start
    (at A)
   )

   (:goal 
       (and 
        (at A)
        
        ;;; (visited A)
        ;;; (visited B)
        ;;; (visited C)
        ;;; (visited D)
        ;;; (visited E)
        ;;; (visited F)
        ;;; (visited G)
        ;;; (visited H)
        ;;; (visited I)

        (forall (?node - node) (visited ?node))
        )

       )
)
