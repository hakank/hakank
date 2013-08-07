;;;
;;; 7,1,8,9,12,5,3,10,4,11,6,2
;;; r,s,s,s,s,r,s,s,r,r,s
;;;
;;; Solved by Prolog-Planning-Library in 0.188s
;;; using forward-iddfs.pl
;;;
(define (problem m12-06)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        ;;; 7,1,8,9,12,5,3,10,4,11,6,2
        (pos1 v7)
        (pos2 v1)
        (pos3 v8)
        (pos4 v9)
        (pos5 v12)
        (pos6 v5)
        (pos7 v3)
        (pos8 v10)
        (pos9 v4)
        (pos10 v11)
        (pos11 v6)
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
