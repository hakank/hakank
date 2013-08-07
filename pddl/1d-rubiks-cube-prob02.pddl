;;;
;;;  6 5 4 3 1 2
;;;
(define (problem rubik-02)
   (:domain rubik-1d)
   (:objects v1 v2 v3 v4 v5 v6)
   (:init
        (pos1 v6)
        (pos2 v5)
        (pos3 v4)
        (pos4 v3)
        (pos5 v1)
        (pos6 v2)

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
