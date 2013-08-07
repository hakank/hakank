;;;
;;; 8,11,6,1,10,9,4,3,12,7,2,5
;;; 
;;;
;;; Solved by Prolog-Planning-Library in 3:52.53 minutes
;;; using forward-iddfs.pl
;;; 
;;; The solution (27 steps)
;; m12-06 231.760 3060042 6520832 27 (inverse v8 v7 v2 v5 v11 v6 v1 v10 v9 v4 v3 v12) (merge v5 v6 v11 v8 v2 v7 v12 v3 v4 v9 v10 v1) (merge v5 v4 v12 v2 v7 v3 v9 v1 v11 v8 v6 v10) (merge v5 v11 v9 v7 v3 v1 v8 v10 v12 v2 v4 v6) (inverse v5 v12 v8 v3 v1 v10 v2 v6 v9 v7 v11 v4) (merge v3 v10 v1 v5 v8 v12 v4 v11 v7 v9 v6 v2) (merge v3 v7 v4 v8 v12 v11 v9 v2 v1 v5 v10 v6) (merge v3 v1 v9 v12 v11 v2 v5 v6 v4 v8 v7 v10) (inverse v3 v4 v5 v11 v2 v6 v8 v10 v9 v12 v1 v7) (merge v11 v6 v2 v3 v5 v4 v7 v1 v12 v9 v10 v8) (merge v11 v12 v7 v5 v4 v1 v9 v8 v2 v3 v6 v10) (merge v11 v2 v9 v4 v1 v8 v3 v10 v7 v5 v12 v6) (merge v11 v7 v3 v1 v8 v10 v5 v6 v9 v4 v2 v12) (inverse v11 v9 v5 v8 v10 v6 v4 v12 v3 v1 v7 v2) (merge v8 v6 v10 v11 v5 v9 v2 v7 v1 v3 v12 v4) (inverse v8 v1 v2 v5 v9 v7 v3 v4 v10 v11 v6 v12) (merge v5 v7 v9 v8 v2 v1 v12 v6 v11 v10 v4 v3) (inverse v5 v11 v12 v2 v1 v6 v10 v3 v9 v8 v7 v4) (merge v2 v6 v1 v5 v12 v11 v4 v7 v8 v9 v3 v10) (merge v2 v8 v4 v12 v11 v7 v9 v10 v1 v5 v6 v3) (inverse v2 v1 v9 v11 v7 v10 v5 v3 v4 v12 v8 v6) (merge v11 v10 v7 v2 v9 v1 v6 v8 v12 v4 v3 v5) (merge v11 v12 v6 v9 v1 v8 v4 v5 v7 v2 v10 v3) (inverse v11 v7 v4 v1 v8 v5 v2 v3 v6 v9 v12 v10) (merge v1 v5 v8 v11 v4 v7 v10 v12 v9 v6 v3 v2) (merge v1 v9 v10 v4 v7 v12 v6 v2 v8 v11 v5 v3) (merge v1 v8 v6 v7 v12 v2 v11 v3 v10 v4 v9 v5)

(define (problem m12-07)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        (pos1 v8)
        (pos2 v11)
        (pos3 v6)
        (pos4 v1)
        (pos5 v10)
        (pos6 v9)
        (pos7 v4)
        (pos8 v3)
        (pos9 v12)
        (pos10 v7)
        (pos11 v2)
        (pos12 v5)
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
