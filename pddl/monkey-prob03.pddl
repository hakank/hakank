;;;
;;;
;;; Problem from Graphplan's monkey_facts3
;;; """
;; From UCPOP [which they got from Prodify]
;; Note: this one is unsolvable because once you get up onto a box, which
;; is needed for either getting water or getting bananas, there is no
;; operator for getting back down off the box which you need to do to get
;; the other one.
;;; """
;;;
;;; hakank: This is unsolvable with monkey-domain.pddl
;;;         but solvable with monkey-domain2.pddl (which has
;;;         the missing (step-down ?x) action.
;;;
(define (problem monkey-03)
   (:domain monkeyproblem)
   (:objects p1 p2 p3 p4 p5 p6)
   (:init
    
     (at monkey p1)
     (on-floor)
     (at box p2)
     (at bananas p3)
     (at knife p4)
     (at waterfountain p5) ;;; <- different from monkey-prob02.pddl
     (at glass p6)

   )

   (:goal 
       (and 
        (hasbananas)
        (haswater)
       )
       )
)
