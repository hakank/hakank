;;;
;;; 1, 7, 12, 6, 2, 8, 11, 5, 3, 9, 10, 4
;;; (3 steps)
;;;;
(define (problem m12-03)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        ;;; 1, 7, 12, 6, 2, 8, 11, 5, 3, 9, 10, 4
        ;;; (3 steps)
        (pos1 v1)
        (pos2 v7)
        (pos3 v12)
        (pos4 v6)
        (pos5 v2)
        (pos6 v8)
        (pos7 v11)
        (pos8 v5)
        (pos9 v3)
        (pos10 v9)
        (pos11 v10)
        (pos12 v4)

   )

   (:goal (and 
       (pos1 v1)
       (pos2 v2)
       (pos3 v3)    
       (pos4 v4)    
       (pos5 v5)    
       (pos6 v6)   
       (pos7 v7)
       (pos8 v8)
       (pos9 v9)
       (pos10 v10)
       (pos11 v11)
       (pos12 v12)
       ))

   ;;; (:length (:serial 10) (:parallel 10))
)
