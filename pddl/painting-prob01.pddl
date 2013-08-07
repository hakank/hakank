;;;
;;; 
;;; 
(define (problem painting-01)
   (:domain painting)
   (:objects c1 c2 red blue r1 r2 r3 r4 b1 b2 b3 b4)
   (:init
       (can c1)
       (can c2)
       (color c1 red)
       (color c2 blue)
       (brush r1)
       (brush r2)
       (dry r1)
       (dry r2)
       (dry r3)
       (dry r4)
       (block b1)
       (block b2)
       (block b3)
       (block b4)
       
   )

   (:goal 
      (and 
         (color b1 red)
         (color b2 red)
         (color b3 blue)    
         (color b4 blue)    
       )
   )

   ;;; (:length (:serial 10) (:parallel 10))
)
