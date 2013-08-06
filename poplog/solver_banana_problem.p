/*
  
   GPS Banana problem in Pop-11.

   Banana problem from Chapter 4 of 
   Peter Norvig's Paradigms of Artificial Intelligence Programming (PAIP)

   The comments in quotes ("...") is also from this book.
   
   Notes: 
    * The order of a rule are
      - name (action)
      - preconditions
      - delete rule
      - add rule
  
      In PAIP, the two last items are in the reversed order.

    * solver uses VED for showing the progression, but it kinda works
      when running the file from command line. 
    * The program finishes with 'bye'
    * The solution is written in the file ./tree.
   

   For more about solver, see 
     * TEACH SOLVER
   which uses the block world as an example.


   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/

*/

lib solver;

[
    [[climb_on_chair]
     [[chair_at_middle_room] [at_middle_room] [on_floor]]
     [[at_middle_room] [on_floor]]
     [[at_bananas] [on_chair]]
     ]
    
    [[push_chair_from_door_to_middle_room]
     [[chair_at_door] [at_door]]
     [[chair_at_door] [at_door]]
     [[chair_at_middle_room] [at_middle_room]]
     ]
    
    [[walk_from_door_to_middle_room]
     [[at_door] [on_floor]]
     [[at_door]]
     [[at_middle_room]]
     ]
    
    [[grasp_bananas]
     [[at_bananas] [empty_handed]]
     [[empty_handed]]
     [[has_bananas]]
     ]
    
    [[drop_ball]
     [[has_ball]]
     [[has_ball]]
     [[empty_handed]]
     ]

    [[eat_bananas]
     [[has_bananas] [hungry]] ;;; hakank: added hungry as precondition
     [[has_bananas] [hungry]]
     [[empty_handed] [not_hungry]]
    ] 
] -> schemalist;




;;; I don't know whether these really works...
5 -> lookahead;  ;;; default 2
false -> clever; ;;; default false
true -> verbose; ;;; default true. 
false -> noloops; ;;; default false
false -> noclobber; ;;; default false
false -> estimating; ;;; default false

'database:',database==>;
'schemalist:',schemalist==>;

;;; no delay
0 -> solverdelay;

'Note: This is using VED!'=>;

[[at_door] [on_floor] [has_ball] [hungry] [chair_at_door]] -> database;
[[not_hungry]] -> lastgoals;

/*
COMPLETE PLAN IS:
    [push_chair_from_door_to_middle_room]
    [climb_on_chair]
    [drop_ball]
    [grasp_bananas]
    [eat_bananas]
*/

runsolver(runastar);
;;; runsolver(runstrips);

'\n\n\nThe result has been written to the file \'tree\''=>;

bye;
