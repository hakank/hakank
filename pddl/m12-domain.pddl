;;;
;;; M12 Problem
;;;
;;; See 
;;; Igor Kriz and Paul Siegel: 
;;;     Rubik's Cube Inspired Puzzles Demonstrate Math's "Simple Groups"
;;; http://www.sciam.com/article.cfm?id=simple-groups-at-play
;;;
;;; Programs:
;;;  http://www.math.lsa.umich.edu/~ikriz/
;;;  http://www.sciam.com/article.cfm?id=puzzles-simple-groups-at-play
;;;
;;; This model implements the M12 puzzle:
;;;  - length is 12 (2*6)
;;;  - the two operations are 
;;;      * merge (shuffle) and 
;;;      * inverse (reverse)
;;;  - some init configuration
;;;
;;;
;;; For a group theoretic solution of the M12 puzzle using the abstract algebra system GAP, 
;;; see http://www.hakank.org/group_theory/M12_gap.txt
;;; It is presented in the (swedish) blog post
;;; "Gruppteoretisk lösning av M12 puzzle i GAP" (Group theoretical solution of the M12 puzzle in GAP)
;;; http://www.hakank.org/webblogg/archives/001226.html

;;;
;;; Also see http://hakank.org/minizinc/M12.mzn
;;;
(define (domain M12)
   (:requirements :adl :typing)
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
                )

   ;
   ; This will change 
   ;    1 12  2 11  3 10  4  9  5  8  6  7  to
   ;    1  2  3  4  5  6  7  8  9 10 11 12 
   ; or rather
   ;    1  2  3  4  5  6  7  8  9 10 11 12 ->
   ;    1  3  5  7  9 11  12 10 8  6  4  2
   ; 
   (:action merge
             :parameters (?v1 ?v2 ?v3 ?v4 ?v5 ?v6 ?v7 ?v8 ?v9 ?v10 ?v11 ?v12 )
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
                          )

             :effect 
                       (and
                           (pos1 ?v1)              
                           (pos2 ?v3)              
                           (pos3 ?v5)              
                           (pos4 ?v7)              
                           (pos5 ?v9)              
                           (pos6 ?v11)              
                           (pos7 ?v12)              
                           (pos8 ?v10)              
                           (pos9 ?v8)              
                           (pos10 ?v6)              
                           (pos11 ?v4)              
                           (pos12 ?v2)
                           (not (pos1 ?v1))
                           (not (pos2 ?v2))
                           (not (pos3 ?v3))
                           (not (pos4 ?v4))
                           (not (pos5 ?v5))
                           (not (pos6 ?v6))
                           (not (pos7 ?v7))
                           (not (pos8 ?v8))
                           (not (pos9 ?v9))
                           (not (pos10 ?v10))
                           (not (pos11 ?v11))
                           (not (pos12 ?v12))
                           )

    ) 

    ;
    ;     1 2 3 4 5 6 7 8 9 10 11 12 -->
    ;    12 11 10 9 8 7 6 5 4 3 2 1
    ;    
   (:action inverse
             :parameters (?v1 ?v2 ?v3 ?v4 ?v5 ?v6 ?v7 ?v8 ?v9 ?v10 ?v11 ?v12  )
             :precondition
                           (and (pos1 ?v1) 
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
                                )

             :effect  
                       (and 
                           (pos1 ?v12)
                           (pos2 ?v11)              
                           (pos3 ?v10)              
                           (pos4 ?v9)              
                           (pos5 ?v8)
                           (pos6 ?v7)
                           (pos7 ?v6)
                           (pos8 ?v5)
                           (pos9 ?v4)
                           (pos10 ?v3)
                           (pos11 ?v2)
                           (pos12 ?v1)
                           (not (pos1 ?v1))
                           (not (pos2 ?v2))
                           (not (pos3 ?v3))
                           (not (pos4 ?v4))
                           (not (pos5 ?v5))
                           (not (pos6 ?v6))
                           (not (pos7 ?v7))
                           (not (pos8 ?v8))
                           (not (pos9 ?v9))
                           (not (pos10 ?v10))
                           (not (pos11 ?v11))
                           (not (pos12 ?v12))
                           )
    ) 



)
