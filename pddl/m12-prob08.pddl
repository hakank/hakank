;;;
;;; 3,8,6,12,4,7,5,11,1,10,9,2
;;; 
;;;
;;; Solved by Prolog-Planning-Library in 5.628s
;;; using forward-iddfs.pl
;;; 19 steps

(define (problem m12-08)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        (pos1 v3)
        (pos2 v8)
        (pos3 v6)
        (pos4 v12)
        (pos5 v4)
        (pos6 v7)
        (pos7 v5)
        (pos8 v11)
        (pos9 v1)
        (pos10 v10)
        (pos11 v9)
        (pos12 v2)
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
