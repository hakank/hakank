;;;
;;;  2 1 4 2 6 5
;;;
;;; Unsolvable:
;;;  FF: 4.69s
;;;
(define (problem rubik-05)
   (:domain rubik-1d)
   (:objects v1 v2 v3 v4 v5 v6)
   (:init
        (pos1 v2)
        (pos2 v1)
        (pos3 v4)
        (pos4 v3)
        (pos5 v6)
        (pos6 v5)

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
