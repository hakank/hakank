;;;
;;;  1 3 2 6 5 4
;;;
(define (problem rubik-01)
   (:domain rubik-1d)
   (:objects v1 v2 v3 v4 v5 v6)
   (:init
        (pos1 v1)
        (pos2 v3)
        (pos3 v2)
        (pos4 v6)
        (pos5 v5)
        (pos6 v4)

   )
   (:goal (and 
       (pos1 v1)
       (pos2 v2)
       (pos3 v3)    
       (pos4 v4)    
       (pos5 v5)    
       (pos6 v6)    
       ))
   ;;;(:length (:serial 13) (:parallel 13))
)
