;;;
;;; Washing problem in PDDL.
;;;
;;; From Automated Planning, page 107
;;;
(define (domain washing)
   (:requirements :strips)
   (:predicates (start-fill ?x)
                (end-fill ?x)
                (start-wash ?x)
                (end-wash ?x ?y)
                (status ?x ?y)
                (use ?x ?y)
                (clean ?x ?y)
                (loc ?x ?y)
                )

   (:constants true false ready fill full wash ready water)

   (:action start-fill
             :parameters (?x)
             :precondition 
                       (and 
                           (status ?x ready)
                           (use water false)
                          )

             :effect 
                       (and
                          (status ?x fill)
                          (use water true)
                          (not (status ?x ready))
                          (not (use water false))
                           )

    ) 

   (:action end-fill
             :parameters (?x)
             :precondition 
                       (and 
                           (status ?x fill)
                          )

             :effect 
                       (and
                          (status ?x full)
                          (use water false)
                          (not (status ?x fill))
                           )

    ) 

   (:action start-wash
             :parameters (?x)
             :precondition 
                       (and 
                           (status ?x full)
                          )

             :effect 
                       (and
                          (status ?x wash)
                          (not (status ?x full))
                           )

    ) 


   (:action end-wash
             :parameters (?x ?y)
             :precondition 
                       (and 
                           (status ?x wash)
                          )

             :effect 
                       (and
                          (status ?x ready)
                          (clean ?y true)
                          (not (status ?x wash))
                           )

    ) 


)
