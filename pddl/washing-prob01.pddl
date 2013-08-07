;;;
;;; Washing
;;; 
(define (problem washing-01)
   (:domain washing)
   (:objects dw wm bt dan clothes dishes)
   (:init
        (status dw ready)
        (status wm ready)
        (status bt ready)
        (clean dan false)
        (clean clothes false)
        (clean dishes false)
        (loc dishes dw)
        (loc clothes wm)
        (loc dan bt)
        (use water false)
   )

   (:goal 
      (and 
          (clean clothes true)
          (clean dishes true)
          (clean dan true)
       )
   )

   ;;; (:length (:serial 10) (:parallel 10))
)
