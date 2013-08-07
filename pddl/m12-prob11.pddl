;;;
;;; 10,5,4,7,1,2,8,3,12,11,9,6
;;; 
;;;
;;; Prolog-Planning-Library, using forward-iddfs.pl:
;;; 1.071s: 15steps
;;;
(define (problem m12-11)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        (pos1 v10)
        (pos2 v5)
        (pos3 v4)
        (pos4 v7)
        (pos5 v1)
        (pos6 v2)
        (pos7 v8)
        (pos8 v3)
        (pos9 v12)
        (pos10 v11)
        (pos11 v9)
        (pos12 v6)
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
