/*
  
   Block world problem in Pop-11.

   This example is (in principle) verbatim from TEACH SOLVER.

    * solver uses VED for showing the progression, but it kinda works
      when running the file from command line. 
    * The program finishes with 'bye'
    * The solution is written in the file ./tree.
   
   For more about solver, see 
     * TEACH SOLVER


   This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');
uses teaching;
lib solver;


;;;
;;; schemalist for block world from TEACH SOLVER
;;;
vars X, Y;
[
 [[take ?X off table]
  [[emptyhand] [cleartop ?X] [ontable ?X]]
  [[emptyhand] [ontable ?X]]
  [[holding ?X]]]
 [[place ?X on table]
  [[holding ?X]]
  [[holding ?X]]
  [[ontable ?X] [emptyhand]]]
 [[pick up ?X from ?Y]
  [[emptyhand] [?X on ?Y] [cleartop ?X]]
  [[emptyhand] [?X on ?Y]]
  [[holding ?X] [cleartop ?Y]]]
 [[put ?X on ?Y]
  [[holding ?X] [cleartop ?Y]]
  [[holding ?X] [cleartop ?Y]]
  [[emptyhand] [?X on ?Y]]]
 ] -> schemalist;


;;;
;;; database for block world
;;;
[[ontable b1]
 [b2 on b1] [cleartop b2]
 [holding b3] [cleartop b3]
 [ontable b4] [cleartop b4]
 [ontable b5] [cleartop b5]
 ] -> database;

;;; I don't know whether these really works...
2 -> lookahead;  ;;; default 2
true -> clever; ;;; default false
false -> verbose; ;;; default true. 
true -> noloops; ;;; default false
false -> noclobber; ;;; default false
false -> estimating; ;;; default false

'database:',database==>;
'schemalist:',schemalist==>;

;;; no delay
0 -> solverdelay;

'Note: This is using VED!'=>;


;;; [[b1 on b2] [b3 on b4] [holding b5]] -> lastgoals;
;;; [[b1 on b2] [b2 on b3] [b3 on b4] [b4 on b5]] -> lastgoals;
[[ontable b2]] -> lastgoals;

runsolver(runastar);;
;;; runsolver(runstrips);


bye;