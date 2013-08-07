;;;
;;; Monkey problem in PDDL.
;;;
;;; From Graphplan example monkey_ops.
;;; """
;;; From UCPOP [which they got from Prodigy].  Note: there is no operator 
;;; to get back down from a box...
;;; """
;;;
;;; hakank: I added step-down (from box)
;;;
(define (domain monkeyproblem)
   (:requirements :adl)
   (:constants monkey box knife bananas waterfountain glass)
   (:predicates 
                 (goto ?x ?y) 
                 (climb ?x)
                 (push-box ?x ?y)
                 (get-knife ?y)
                 (grab-bananas ?y)
                 (pickglass ?y)
                 (getwater ?y)
                 (on-floor)
                 (at ?x ?y)
                 (hasbananas)
                 (hasknife)
                 (hasglass)
                 (haswater)
                 (onbox ?x)
                 
                 ;;; hakank: added this
                 (step-down ?x)

                 )

   (:action goto
             :parameters (?x ?y)
             :precondition (and 
                             (on-floor)
                             (at monkey ?y)
                             )
             :effect  (and 
                           (at monkey ?x)
                           (not (at monkey ?y))
                           )
    ) 


   (:action climb
             :parameters (?x)
             :precondition (and 
                             (at box ?x)
                             (at monkey ?x)
                             )
             :effect  (and 
                           (onbox ?x)
                           (not (on-floor))
                           )
    ) 

   (:action step-down
             :parameters (?x)
             :precondition (and 
                             (onbox ?x)
                             (at monkey ?x)
                             )
             :effect  (and 
                           (on-floor)
                           (not (onbox ?x))
                           )
    ) 


   (:action push-box
             :parameters (?x ?y)
             :precondition (and 
                             (at box ?y)
                             (at monkey ?y)
                             (on-floor)
                             )
             :effect  (and 
                           (at monkey ?x)
                           (at box ?x)
                           (not (at monkey ?y))
                           (not (at box ?y))
                           )
    ) 

   (:action get-knife
             :parameters (?y)
             :precondition (and 
                             (at knife ?y)
                             (at monkey ?y)
                             )
             :effect  (and 
                           (hasknife)
                           (not (at knife ?y))
                           )
    ) 

   (:action grab-bananas
             :parameters (?y)
             :precondition (and 
                             (hasknife)
                             (at bananas ?y)
                             (onbox ?y)
                             )
             :effect  (and 
                           (hasbananas)
                           )
    ) 

   (:action pickglass
             :parameters (?y)
             :precondition (and 
                             (at glass ?y)
                             (at monkey ?y)
                             )
             :effect  (and 
                           (hasglass)
                           (not (at glass ?y))
                           )
    ) 

   (:action getwater
             :parameters (?y)
             :precondition (and 
                             (hasglass)
                             (at waterfountain ?y)
                             (at monkey ?y)
                             (onbox ?y)
                             )
             :effect  (and 
                           (haswater)
                           )
    ) 

)

