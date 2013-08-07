;;;
;;; Rotation permutation puzzle in PDDL.
;;;
;;; From GAP mailing list
;;; http://www-groups.dcs.st-and.ac.uk/~gap/ForumArchive/Harris.1/Bob.1/Re__GAP_.59/1.html
;;; """
;;; Since you asked about what the puzzle actually is, the fellow who posted it
;;; at rec.puzzles (Kevin Buzzard <buzzard@ic.PUZZLE.ac.ELZZUP.uk>) had found it
;;; on his nokia cell phone. It is called "rotation" at Nokia's web site
;;; http://www.nokia.com/games
;;; I think this variant is level 5.
;;;
;;; The puzzle is a 4x4 square of numbers. There are four operations, each of
;;; which involves rotating the numbers in a 3x3 square clockwise. So, in the
;;; diagram below, one move is the cycle (1,2,3,7,11,10,9,5). The numbers
;;; maintain orientation-- they don't rotate; if they did, that would add
;;; another layer of complexity for the solver.
;;;
;;;      1  2  3  4
;;;      5  6  7  8
;;;      9 10 11 12
;;;     13 14 15 16
;;;
;;; Anyone who's interested in an archive of the discussion ofthis puzzle (it's
;;; about a 40K byte text file), let me know. The discussion focuses primarily
;;; on finding minimal move sequences to swap two given tiles.
;;;
;;; There's a very well done java applet for a similar puzzle at
;;; http://www.microprizes.com/mp52.htm 
;;; """

;;;
;;; Also see http://hakank.org/minizinc/rotation.mzn
;;;
(define (domain rotation)
   (:requirements :strips)
   (:predicates (pos1 ?v )
                (pos2 ?v )
                (pos3 ?v )
                (pos4 ?v )
                (pos5 ?v )
                (pos6 ?v )
                (pos7 ?v )
                (pos8 ?v )
                (pos9 ?v )
                (pos10 ?v )
                (pos11 ?v )
                (pos12 ?v )
                (pos13 ?v )
                (pos14 ?v )
                (pos15 ?v )
                (pos16 ?v )
                )

   ;
   ; Rotation: 1, 2, 3,  7, 11, 10,  9,  5
   ;
   ;           1  2   3  4   5   6   7   8   9   10   11   12   13   14  15   16 ->
   ;           5  1   2  4   9   6   3   8  10   11    7   12   13   14  15   16 ->
   ;           _  _   _      _       _      __   __   __

   ; 
   (:action rot1
             :parameters (?v1 ?v2 ?v3 ?v4 ?v5 ?v6 ?v7 ?v8 ?v9 ?v10 ?v11 ?v12 ?v13 ?v14 ?v15 ?v16 )
             :precondition 
                       (and 
                          (pos1 ?v1) 
                          (pos2 ?v2) 
                          (pos3 ?v3) 
                          (pos4 ?v4) 
                          (pos5 ?v5) 
                          (pos6 ?v6) 
                          (pos7 ?v7) 
                          (pos8 ?v8) 
                          (pos9 ?v9) 
                          (pos10 ?v10) 
                          (pos11 ?v11) 
                          (pos12 ?v12)
                          (pos13 ?v13)
                          (pos14 ?v14)
                          (pos15 ?v15)
                          (pos16 ?v16)
                          )

             :effect 
                       (and
                           (pos1 ?v5)
                           (pos2 ?v1)
                           (pos3 ?v2)
                           ;;;(pos4 ?v4)        
                           (pos5 ?v9)
                           ;;;(pos6 ?v6)              
                           (pos7 ?v3)
                           ;;;(pos8 ?v8)              
                           (pos9 ?v10)
                           (pos10 ?v11)
                           (pos11 ?v7)
                           ;;;(pos12 ?v12)
                           ;;;(pos13 ?v13)
                           ;;;(pos14 ?v14)
                           ;;;(pos15 ?v15)
                           ;;;(pos16 ?v16)
                           (not (pos1 ?v1))
                           (not (pos2 ?v2))
                           (not (pos3 ?v3))
                           ;;: (not (pos4 ?v4))
                           (not (pos5 ?v5))
                           ;;;(not (pos6 ?v6))
                           (not (pos7 ?v7))
                           ;;;(not (pos8 ?v8))
                           (not (pos9 ?v9))
                           (not (pos10 ?v10))
                           (not (pos11 ?v11))
                           ;;;(not (pos12 ?v12))
                           ;;;(not (pos13 ?v13))
                           ;;;(not (pos14 ?v14))
                           ;;;(not (pos15 ?v15))
                           ;;;(not (pos16 ?v16))
                           )

    ) 

   ;;
   ;;  rot2:  2, 3, 4,  8, 12, 11, 10,  6,
   ;;
   ;           1  2   3  4   5   6   7   8   9   10   11   12   13   14  15   16 ->
   ;           1  6   2  3   5  10   7   4   9   11   12    8   13   14  15   16 ->          
   ; 
   (:action rot2
             :parameters (?v1 ?v2 ?v3 ?v4 ?v5 ?v6 ?v7 ?v8 ?v9 ?v10 ?v11 ?v12 ?v13 ?v14 ?v15 ?v16 )
             :precondition 
                       (and 
                          (pos1 ?v1) 
                          (pos2 ?v2) 
                          (pos3 ?v3) 
                          (pos4 ?v4) 
                          (pos5 ?v5) 
                          (pos6 ?v6) 
                          (pos7 ?v7) 
                          (pos8 ?v8) 
                          (pos9 ?v9) 
                          (pos10 ?v10) 
                          (pos11 ?v11) 
                          (pos12 ?v12)
                          (pos13 ?v13)
                          (pos14 ?v14)
                          (pos15 ?v15)
                          (pos16 ?v16)
                          )

             :effect 
                       (and
                          ;;;(pos1 ?v1) 
                          (pos2 ?v6) 
                          (pos3 ?v2) 
                          (pos4 ?v3) 
                          ;;;(pos5 ?v5) 
                          (pos6 ?v10) 
                          ;;;(pos7 ?v7) 
                          (pos8 ?v4) 
                          ;;;(pos9 ?v9) 
                          (pos10 ?v11) 
                          (pos11 ?v12) 
                          (pos12 ?v8)
                          ;;;(pos13 ?v13)
                          ;;;(pos14 ?v14)
                          ;;;(pos15 ?v15)
                          ;;;(pos16 ?v16)

                           ;;;;(not (pos1 ?v1))
                           (not (pos2 ?v2))
                           (not (pos3 ?v3))
                           (not (pos4 ?v4))
                           ;;;(not (pos5 ?v5))
                           (not (pos6 ?v6))
                           ;;;(not (pos7 ?v7))
                           (not (pos8 ?v8))
                           ;;;(not (pos9 ?v9))
                           (not (pos10 ?v10))
                           (not (pos11 ?v11))
                           ;; (not (pos12 ?v12))
                           ;; (not (pos13 ?v13))
                           ;; (not (pos14 ?v14))
                           ;; (not (pos15 ?v15))
                           ;; (not (pos16 ?v16))
                           )

    ) 


   ;;
   ;;  rot3:  5, 6, 7, 11, 15, 14, 13,  9,
   ;;;
   ;           1  2   3  4   5   6   7   8   9   10   11   12   13   14  15   16 ->
   ;           1  2   3  4   9   5   6   8  13   10    7   12   14   15  11   16 
   ;;
   (:action rot3
             :parameters (?v1 ?v2 ?v3 ?v4 ?v5 ?v6 ?v7 ?v8 ?v9 ?v10 ?v11 ?v12 ?v13 ?v14 ?v15 ?v16 )
             :precondition 
                       (and 
                          (pos1 ?v1) 
                          (pos2 ?v2) 
                          (pos3 ?v3) 
                          (pos4 ?v4) 
                          (pos5 ?v5) 
                          (pos6 ?v6) 
                          (pos7 ?v7) 
                          (pos8 ?v8) 
                          (pos9 ?v9) 
                          (pos10 ?v10) 
                          (pos11 ?v11) 
                          (pos12 ?v12)
                          (pos13 ?v13)
                          (pos14 ?v14)
                          (pos15 ?v15)
                          (pos16 ?v16)
                          )

             :effect 
                       (and
                          ;; (pos1 ?v1) 
                          ;; (pos2 ?v2) 
                          ;; (pos3 ?v3) 
                          ;; (pos4 ?v4) 
                          (pos5 ?v9) 
                          (pos6 ?v5) 
                          (pos7 ?v6) 
                          ;;;(pos8 ?v8) 
                          (pos9 ?v13) 
                          ;;(pos10 ?v10) 
                          (pos11 ?v7) 
                          ;;(pos12 ?v12)
                          (pos13 ?v14)
                          (pos14 ?v15)
                          (pos15 ?v11)
                          ;;;(pos16 ?v16)

                           ;; (not (pos1 ?v1))
                           ;; (not (pos2 ?v2))
                           ;; (not (pos3 ?v3))
                           ;; (not (pos4 ?v4))
                           (not (pos5 ?v5))
                           (not (pos6 ?v6))
                           (not (pos7 ?v7))
                           ;; (not (pos8 ?v8))
                           (not (pos9 ?v9))
                           ;;;(not (pos10 ?v10))
                           (not (pos11 ?v11))
                           ;;;(not (pos12 ?v12))
                           (not (pos13 ?v13))
                           (not (pos14 ?v14))
                           (not (pos15 ?v15))
                           ;;;(not (pos16 ?v16))
                           )

    ) 

   ;;
   ;;  rot4:  6, 7, 8, 12, 16, 15, 14, 10,
   ;;
   ;          1  2   3  4   5   6   7   8   9   10   11   12   13   14  15   16 ->
   ;          1  2   3  4   5  10   6   7   9   14   11    8   13   15  16   12 
   ;;
   (:action rot4
             :parameters (?v1 ?v2 ?v3 ?v4 ?v5 ?v6 ?v7 ?v8 ?v9 ?v10 ?v11 ?v12 ?v13 ?v14 ?v15 ?v16 )
             :precondition 
                       (and 
                          (pos1 ?v1) 
                          (pos2 ?v2) 
                          (pos3 ?v3) 
                          (pos4 ?v4) 
                          (pos5 ?v5) 
                          (pos6 ?v6) 
                          (pos7 ?v7) 
                          (pos8 ?v8) 
                          (pos9 ?v9) 
                          (pos10 ?v10) 
                          (pos11 ?v11) 
                          (pos12 ?v12)
                          (pos13 ?v13)
                          (pos14 ?v14)
                          (pos15 ?v15)
                          (pos16 ?v16)
                          )

             :effect 
                       (and
                          ;; (pos1 ?v1) 
                          ;; (pos2 ?v2) 
                          ;; (pos3 ?v3) 
                          ;; (pos4 ?v4) 
                          ;; (pos5 ?v5) 
                          (pos6 ?v10) 
                          (pos7 ?v6) 
                          (pos8 ?v7) 
                          ;;;(pos9 ?v9) 
                          (pos10 ?v14) 
                          ;;;(pos11 ?v11) 
                          (pos12 ?v8)
                          ;;;(pos13 ?v13)
                          (pos14 ?v15)
                          (pos15 ?v16)
                          (pos16 ?v12)

                           ;; (not (pos1 ?v1))
                           ;; (not (pos2 ?v2))
                           ;; (not (pos3 ?v3))
                           ;; (not (pos4 ?v4))
                           ;; (not (pos5 ?v5))
                           (not (pos6 ?v6))
                           (not (pos7 ?v7))
                           (not (pos8 ?v8))
                           ;;;(not (pos9 ?v9))
                           (not (pos10 ?v10))
                           ;;;(not (pos11 ?v11))
                           (not (pos12 ?v12))
                           ;;;(not (pos13 ?v13))
                           (not (pos14 ?v14))
                           (not (pos15 ?v15))
                           (not (pos16 ?v16))
                           )

    ) 



)
