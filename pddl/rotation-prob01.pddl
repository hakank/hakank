;;;
;;; 
;;; 
;;; One 1-rotation:
;;  [2, 3, 7, 4, 1, 6, 11, 8, 5,  9, 10, 12, 13, 14, 15, 16

;;;
;;; Prolog-Planning-Library, using forward-iddfs.pl:
;;; 0.101s, 1 step
;;;
(define (problem rotation-01)
   (:domain rotation)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
   (:init

        ;;; One 1-rotation:
        ;; 2, 3, 7, 4, 1, 6, 11, 8, 5,  9, 10, 12, 13, 14, 15, 16
        (pos1 v2)
        (pos2 v3)
        (pos3 v7)
        (pos4 v4)
        (pos5 v1)
        (pos6 v6)
        (pos7 v11)
        (pos8 v8)
        (pos9 v5)
        (pos10 v9)
        (pos11 v10)
        (pos12 v12)
        (pos13 v13)
        (pos14 v14)
        (pos15 v15)
        (pos16 v16)
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
