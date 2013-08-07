;;;
;;; 
;;; 
;; 1,2,3,2,2,3
;; 8, 12, 7, 15, 1, 4, 2, 10, 3, 6, 11, 14, 5, 9, 13, 16


;;;
;;; Prolog-Planning-Library, using forward-iddfs.pl:
;;; 1.097s  6 steps 
;;;
(define (problem rotation-05)
   (:domain rotation)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
   (:init

        ;; op: 1,2,3,2,2,3
        ;; 8, 12, 7, 15, 1, 4, 2, 10, 3, 6, 11, 14, 5, 9, 13, 16
        (pos1 v8)
        (pos2 v12)
        (pos3 v7)
        (pos4 v15)
        (pos5 v1)
        (pos6 v4)
        (pos7 v2)
        (pos8 v10)
        (pos9 v3)
        (pos10 v6)
        (pos11 v11)
        (pos12 v14)
        (pos13 v5)
        (pos14 v9)
        (pos15 v13)
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
