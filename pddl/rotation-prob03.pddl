;;;
;;; 
;;; 
;; 1,2,1
;; 7,4,11,8,2,3,9,12,1,5,6,10,13,14,15,16

;;;
;;; Prolog-Planning-Library, using forward-iddfs.pl:
;;; 0.101s, 3 steps
;;;
(define (problem rotation-03)
   (:domain rotation)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
   (:init

        ;; op: 1,2,1
        ;; 7,4,11,8,2,3,9,12,1,5,6,10,13,14,15,16

        (pos1 v7)
        (pos2 v4)
        (pos3 v11)
        (pos4 v8)
        (pos5 v2)
        (pos6 v3)
        (pos7 v9)
        (pos8 v12)
        (pos9 v1)
        (pos10 v5)
        (pos11 v6)
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
