;;;
;;;  4 1 10 7 9 12 3 6 5 2 11 8
;;;;
(define (problem m12-01)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        ;; 4 1 10 7 9 12 3 6 5 2 11 8
        (pos1 v4)
        (pos2 v1)
        (pos3 v10)
        (pos4 v7)
        (pos5 v9)
        (pos6 v12)
        (pos7 v3)
        (pos8 v6)
        (pos9 v5)
        (pos10 v2)
        (pos11 v11)
        (pos12 v8)

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
