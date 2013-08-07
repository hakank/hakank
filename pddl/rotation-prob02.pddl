;;;
;;  2,1
;; 2, 7, 4, 8, 1, 3, 11, 12, 5, 6, 9, 10, 13, 14, 15, 16
;;;

;;;
;;; Prolog-Planning-Library, using forward-iddfs.pl:
;;; 0.108s, 2 steps 
;;; 
(define (problem rotation-02)
   (:domain rotation)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
   (:init

        ;; op: 2,1
        ;; 2, 7, 4, 8, 1, 3, 11, 12, 5, 6, 9, 10, 13, 14, 15, 16
        (pos1 v2)
        (pos2 v7)
        (pos3 v4)
        (pos4 v8)
        (pos5 v1)
        (pos6 v3)
        (pos7 v11)
        (pos8 v12)
        (pos9 v5)
        (pos10 v6)
        (pos11 v9)
        (pos12 v10)
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
