/*
  
   GPS School problem in Pop-11.

   School problem from Chapter 4 of 
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

vars X,Y;
[
 ;;; slighly more general
 [[drive ?X to school]
  [[?X at home] [car works]]
  [[?X at home]]
  [[?X at school]]
  ]
   
 [[shop installs battery]
  [[car needs battery] [shop knows problem] [shop has money]]
  [[car needs battery]]
  [[car works]]
  ]

 [[tell shop problem]
  [[in communication with shop]]
  [[in communication with shop]]
  [[shop knows problem]]
  ]
  
 [[telephone shop]
  [[know phone number]]
  [[know phone number]]  ;;; hakank: must do this for the solver to work
  [[in communication with shop] [know phone number]] ;;; put the knowledge back
  ]
 
 [[look up number]
  [[have phone book]]
  [[have phone book]]  ;;; hakank: must do this for the solver to work
  [[know phone number] [have phone book]]  ;;; we stil have the phone book...
  ]

 [[give shop money]
  [[have money]]
  [[have money]]
  [[shop has money]]
  ]


] -> schemalist;


;;; "Here are some examples of using GPS"
;;; "The first example works with a complex chain of steps."
[[son at home] [car needs battery] [have money] [have phone book] ] -> database;
[[son at school]] -> lastgoals;

/*
 The plan is:
     [look up number]
     [telephone shop]
     [tell shop problem]
     [give shop money]
     [shop installs battery]
     [drive son to school]

*/

;;; alternative
;;; [[freddy at home] [car needs battery] [have money] [have phone book] ] -> database;
;;; [[freddy at school]] -> lastgoals;


;;; "The next example fails because there is no way to make the car work,"
;;;  "because we can't contact the shop to get the battery fixed."
;;; [[son at home] [car needs battery] [have money]] -> database;
;;; [[son at school]] -> lastgoals;


;;; "The third example is easy, because the car is currently working."
;;; [[son at home] [car works]] -> database;
;;; [[son at school]] -> lastgoals;

;;;  "In the next example, GPS incorrectly reports success, when in fact it has"
;;;  "spent the money on the battery, and thus should fail."
;;; hakank: It seems to be the same problem with SOLVER.
;;; [[son at home] [have money] [car works]] -> database;
;;; [[have money] [son at school]] -> lastgoals;


;;; I don't know whether these really works...
5 -> lookahead;  ;;; default 2
true -> clever; ;;; default false
true -> verbose; ;;; default true. 
false -> noloops; ;;; default false
false -> noclobber; ;;; default false
false -> estimating; ;;; default false

'database:',database==>;
'schemalist:',schemalist==>;

;;; no delay
0 -> solverdelay;

'Note: This is using VED!'=>;

runsolver(runastar);;
;;; runsolver(runstrips);

'\n\n\nThe result has been written to the file \'tree\''=>;

bye;

