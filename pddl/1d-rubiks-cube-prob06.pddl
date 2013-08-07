;;;
;;;  6 3 5 2 4 1
;;;  
;;; This can be done in 11 (12) moves, but FF takes 19 (in 2.08s)
;;;
(define (problem rubik-06)
   (:domain rubik-1d)
   (:objects v1 v2 v3 v4 v5 v6)
   (:init
        (pos1 v6)
        (pos2 v3)
        (pos3 v5)
        (pos4 v2)
        (pos5 v4)
        (pos6 v1)

   )
   (:goal (and 
       (pos1 v1)
       (pos2 v2)
       (pos3 v3)    
       (pos4 v4)    
       (pos5 v5)    
       (pos6 v6)    
       ))
   (:length (:serial 10) (:parallel 10))
)
