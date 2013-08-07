;;;
;;; 12 11 10 9 8 7 6 5 4 3 2 1 (just inverse)
;;;;
(define (problem m12-05)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        ;; 12 11 10 9 8 7 6 5 4 3 2 1 (just inverse)
        ;;
        (pos1 v12)
        (pos2 v11)
        (pos3 v10)
        (pos4 v9)
        (pos5 v8)
        (pos6 v7)
        (pos7 v6)
        (pos8 v5)
        (pos9 v4)
        (pos10 v3)
        (pos11 v2)
        (pos12 v1)
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
