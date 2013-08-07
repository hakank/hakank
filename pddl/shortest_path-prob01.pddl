;;;
;;; """
;;; shortest paths on a 4x4 grid with obstacles
;;; obstacles at 23, 24, and 33.  Start at 11, goal is 44
;;; """
;;;
;;; Graphplan (0.00s)
;; 1 MOVE_11_12
;; 2 MOVE_12_22
;; 3 MOVE_22_32
;; 4 MOVE_32_42
;; 5 MOVE_42_43
;; 6 MOVE_43_44

;; ff (0.00s), declaring both ways
;; step    0: MOVE N11 N21
;;         1: MOVE N21 N22
;;         2: MOVE N22 N32
;;         3: MOVE N32 N42
;;         4: MOVE N42 N43
;;         5: MOVE N43 N44


;; ff (0.00s) declaring just one way (and disjuction in the domain)
;; step    0: MOVE N11 N12
;;         1: MOVE N12 N22
;;         2: MOVE N22 N32
;;         3: MOVE N32 N42
;;         4: MOVE N42 N43
;;         5: MOVE N43 N44


(define (problem shortest_path-01)
   (:domain shortest)
   (:objects n11 n12 n13 n14 n21 n22 n31 n32 n34 n41 n42 n43 n44)
   (:init
    (connected n11 n12)
    ;;;(connected n12 n11)
    (connected n21 n22)
    ;;;;(connected n22 n21)
    (connected n31 n32)
    ;;;;(connected n32 n31)
    (connected n41 n42)
    ;;;(connected n42 n41)

    (connected n12 n13)
    ;;;(connected n13 n12)
    (connected n42 n43)
    ;;;(connected n43 n42)

    (connected n13 n14)
    ;;;(connected n14 n13)
    (connected n43 n44)
    ;;;(connected n44 n43)

    (connected n11 n21)
    ;;;(connected n21 n11)
    (connected n21 n31)
    ;;;(connected n31 n21)
    (connected n31 n41)
    ;;;(connected n41 n31)

    (connected n12 n22)
    ;;;(connected n22 n12)
    (connected n22 n32)
    ;;;(connected n32 n22)
    (connected n32 n42)
    ;;(connected n42 n32)

    (connected n34 n44)
    ;;;(connected n44 n34)

    ;;; start
    (at n11)

   )
   (:goal 
    (and (at n44))
    )
   ;;;(:length (:serial 13) (:parallel 13))
)
