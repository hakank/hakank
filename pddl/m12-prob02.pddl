;;;
;;;  11 2 9 7 1 10 6 5 8 3 12 4 % sssrsss
;;;;
(define (problem m12-02)
   (:domain M12)
   (:objects v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
   (:init

        ;; 11 2 9 7 1 10 6 5 8 3 12 4 % sssrsss
        ;; Prolog-Planning-Library solves this in 0.159s
        ;; using forward-iddfs.pl
        (pos1 v11)
        (pos2 v2)
        (pos3 v9)
        (pos4 v7)
        (pos5 v1)
        (pos6 v10)
        (pos7 v6)
        (pos8 v5)
        (pos9 v8)
        (pos10 v3)
        (pos11 v12)
        (pos12 v4)
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
