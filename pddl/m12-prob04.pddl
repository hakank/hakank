;;;
;;; 1, 12, 2, 11, 3, 10, 4, 9, 5, 8, 6, 7 
;;; 1 step: merge
;;;;
(define (problem m12-04)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        ;; 1, 12, 2, 11, 3, 10, 4, 9, 5, 8, 6, 7
        ;;; 1 step (merge)
        (pos1 v1)
        (pos2 v12)
        (pos3 v2)
        (pos4 v11)
        (pos5 v3)
        (pos6 v10)
        (pos7 v4)
        (pos8 v9)
        (pos9 v5)
        (pos10 v8)
        (pos11 v6)
        (pos12 v7)


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
