;;;
;;;
;;; Problem from Graphplan's monkey_facts1
;;; """
;;; From UCPOP [which they got from Prodify]. 
;;; """
;;;
(define (problem monkey-01)
   (:domain monkeyproblem)
   (:objects p1 p2 p3 p4)
   (:init
    
     (at monkey p1)
     (on-floor)
     (at box p2)
     (at bananas p3)
     (at knife p4)

   )

   (:goal 
       (and 
        (hasbananas)
       )
       )
)
