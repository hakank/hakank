;;;
;;; Painting problem in PDDL.
;;;
;;; From Automated Planning, page 105f
;;;
(define (domain painting)
   (:requirements :strips)
   (:predicates (dip-brush ?r ?v ?c )
                (paint ?b ?r ?k)
                (brush ?r)
                (can ?c)
                (color ?c ?k)
                (canpaint ?r ?k)
                (dry ?r)
                (block ?b)
                )

   (:action dip-brush
             :parameters (?r ?c ?k)
             :precondition 
                       (and 
                           (brush ?r)
                           (can ?c)
                           (color ?c ?k) 
                          )

             :effect 
                       (and
                          (canpaint ?r ?k)
                          (not (dry ?r))
                           )

    ) 


   (:action paint
             :parameters (?b ?r ?k)
             :precondition 
                       (and 
                          (block ?b)     
                          (brush ?r)
                          (canpaint ?r ?k)
                          )

             :effect 
                       (and
                          (color ?b ?k)
                          (not (canpaint ?r ?k))
                          (not (dry ?r))
                           )
    ) 


)
