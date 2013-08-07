;;;
;;; 5,6,11,10,8,2,3,12,7,4,9,1
;;; 
;;;
;;; Prolog-Planning-Library, using forward-iddfs.pl:
;;; 0.428s: 13 steps
;;;
(define (problem m12-10)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        (pos1 v5)
        (pos2 v6)
        (pos3 v11)
        (pos4 v10)
        (pos5 v8)
        (pos6 v2)
        (pos7 v3)
        (pos8 v12)
        (pos9 v7)
        (pos10 v4)
        (pos11 v9)
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
