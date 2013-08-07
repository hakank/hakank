;;; 
;;; Random puzzle
;;; 2,  4, 16, 11, 15,  7, 12,  1, 8,  9, 14,  3,  10,  5, 13, 6


;;;
;;; Prolog-Planning-Library, using forward-iddfs.pl:
;;; timeout (>8.33s)
;;;
(define (problem rotation-07)
   (:domain rotation)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
   (:init

        ;;; 2,  4, 16, 11, 15,  7, 12,  1, 8,  9, 14,  3,  10,  5, 13, 6
        (pos1 v2)
        (pos2 v4)
        (pos3 v16)
        (pos4 v11)
        (pos5 v15)
        (pos6 v7)
        (pos7 v12)
        (pos8 v1)
        (pos9 v8)
        (pos10 v9)
        (pos11 v14)
        (pos12 v3)
        (pos13 v10)
        (pos14 v5)
        (pos15 v13)
        (pos16 v6)
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
       (pos13 v13)
       (pos14 v14)
       (pos15 v15)
       (pos16 v16)
       ))

   ;;; (:length (:serial 10) (:parallel 10))
)
