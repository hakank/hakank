;;;
;;; 1D Rubik's cube
;;;
;;; A variant of 
;;;   http://www.hakank.org/pddl/1d-rubiks-cube-domain.pddl
;;;
;;; This approach use only the predicate 
;;;     (pos position value) 
;;; It works, but is a little slower than the representation 
;;; with (pos1 v), (pos2 v), ...
;;;
;;;
;;; From
;;; http://www.mail-archive.com/programming@jsoftware.com/msg05817.html
;;; """
;;; 1D Rubik's Cube
;;;
;;; Oleg Kobchenko
;;; Mon, 11 Jun 2007 19:09:55 -0700
;;;
;;; I found an interesting game, as found on Andrew Nikitin's 
;;; MSX-BASIC page http://nsg.upor.net/msx/basic/basic.htm ,
;;; and I am not sure if its solver has been given as a puzzle.
;;; Here it goes.
;;;
;;; 1D Rubik's Cube is a line of 6 numbers with
;;; original position:
;;;
;;;   1 2 3 4 5 6
;;;
;;; which can be rotated in 3 different ways
;;; in groups of four:
;;;     _______                _______
;;;    (1 2 3 4)5 6  --(0)->  (4 3 2 1)5 6
;;;       _______                _______
;;;     1(2 3 4 5)6  --(1)->   1(5 4 3 2)6
;;;         _______                _______
;;;     1 2(3 4 5 6) --(2)->   1 2(6 5 4 3)
;;;
;;; Given a scrambled line, return the shortest sequence of 
;;; rotations to restore the original position.
;;;
;;; Examples:
;;;
;;;    solve 1 3 2 6 5 4
;;; 1 2 1
;;;    solve 5 6 2 1 4 3
;;; 0 2
;;;    solve 6 5 4 1 2 3
;;; 0 1 2
;;;
;;; """
;;;
;;; Also see http://hakank.org/minizinc/1d_rubiks_cube.mzn
;;;
;;; This PDDL encoding was done by Hakan Kjellerstrand (hakank@bonetmail.com)
;;; See my PPDL page: http://www.hakank.org/pddl/
;;;
(define (domain rubik-1d2)
   (:requirements :strips :adl :typing)
   ;;;; (:types p - position v - value)
   (:constants p1 p2 p3 p4 p5 p6)
   (:predicates (pos ?p ?v) )

   ;     _______                   _______
   ;    (1 2 3 4)5 6  --(rot0)->  (4 3 2 1)5 6
   ;
   (:action rot0
             :parameters (?v1 ?v2 ?v3 ?v4 ?v5 ?v6)
             :precondition (and (pos p1 ?v1) 
                                (pos p2 ?v2) 
                                (pos p3 ?v3) 
                                (pos p4 ?v4) 
                                (pos p5 ?v5) 
                                (pos p6 ?v6) 
                                )
             :effect  (and 
                           (pos p1 ?v4)              
                           (pos p2 ?v3)              
                           (pos p3 ?v2)              
                           (pos p4 ?v1)              
                           (pos p5 ?v5)              
                           (pos p6 ?v6)      
                           (not (pos p1 ?v1))
                           (not (pos p2 ?v2))
                           (not (pos p3 ?v3))
                           (not (pos p4 ?v4)))
    ) 

    ;
    ;       _______                   _______
    ;     1(2 3 4 5)6  --(rot1)->   1(5 4 3 2)6
    ;
   (:action rot1
             :parameters (?v1 ?v2 ?v3 ?v4 ?v5 ?v6)
             :precondition (and (pos p1 ?v1) 
                                (pos p2 ?v2) 
                                (pos p3 ?v3) 
                                (pos p4 ?v4) 
                                (pos p5 ?v5) 
                                (pos p6 ?v6) 
                                )
             :effect  (and 
                           (pos p1 ?v1)
                           (pos p2 ?v5)              
                           (pos p3 ?v4)              
                           (pos p4 ?v3)              
                           (pos p5 ?v2)
                           (pos p6 ?v6)
                           (not (pos p2 ?v2))
                           (not (pos p3 ?v3))
                           (not (pos p4 ?v4))
                           (not (pos p5 ?v5)))
    ) 

    ;
    ;      _______                   _______
    ;  1 2(3 4 5 6) --(rot2)->   1 2(6 5 4 3)
    ;
    (:action rot2
             :parameters (?v1 ?v2 ?v3 ?v4 ?v5 ?v6)
             :precondition (and (pos p1 ?v1) 
                                (pos p2 ?v2) 
                                (pos p3 ?v3) 
                                (pos p4 ?v4) 
                                (pos p5 ?v5) 
                                (pos p6 ?v6) 
                                )
             :effect  (and 
                           (pos p1 ?v1)
                           (pos p2 ?v2)              
                           (pos p3 ?v6)              
                           (pos p4 ?v5)              
                           (pos p5 ?v4)              
                           (pos p6 ?v3)              
                           (not (pos p3 ?v3))
                           (not (pos p4 ?v4))
                           (not (pos p5 ?v5))
                           (not (pos p6 ?v6)))
    ) 


)
