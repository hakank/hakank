;;;
;;;
;;; Problem from Graphplan's monkey_facts2
;;; """
;;; From UCPOP [which they got from Prodify]. 
;;; """
;;;
(define (problem monkey-02)
   (:domain monkeyproblem)
   (:objects p1 p2 p3 p4 p5 p6)
   (:init
    
     (at monkey p1)
     (on-floor)
     (at box p2)
     (at bananas p3)
     (at knife p4)
     (at waterfountain p3)
     (at glass p6)

   )

   (:goal 
       (and 
        (hasbananas)
        (haswater)
       )
       )
)
