;;;
;;;  1 3 2 6 5 4
;;;
(define (problem rubik-01)
   (:domain rubik-1d2)
   (:objects p1 p2 p3 p4 p5 p6 v1 v2 v3 v4 v5 v6)
   (:init
        (pos p1 v1)
        (pos p2 v3)
        (pos p3 v2)
        (pos p4 v6)
        (pos p5 v5)
        (pos p6 v4)

   )
   (:goal (and 
       (pos p1 v1)
       (pos p2 v2)
       (pos p3 v3)    
       (pos p4 v4)    
       (pos p5 v5)    
       (pos p6 v6)    
       ))
   ;;;(:length (:serial 13) (:parallel 13))
)
