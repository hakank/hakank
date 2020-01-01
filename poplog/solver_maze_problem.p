/*
  
   Maze problem in Pop-11.

   Maze problem from Chapter 4 of 
   Peter Norvig's Paradigms of Artificial Intelligence Programming (PAIP)

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

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');
uses teaching;
lib solver;

vars maze;
[[1 2] [2 3] [3 4] [4 9] [9 14] [9 8] [8 7] [7 12] [12 13]
 [12 11] [11 6] [11 16] [16 17] [17 22] [21 22] [22 23]
 [23 18] [23 24] [24 19] [19 20] [20 15] [15 10] [10 5] [20 25]] -> maze;

;;;
;;; create the schemalist
;;;
vars m, Here, There;
[%for m in maze do 
    m(1) -> Here;
    m(2) -> There;
    [[move from ^Here to ^There]
       [[at ^Here]]
       [[at ^Here]]
       [[at ^There]]
       ];
endfor%] -> schemalist;

[[at 1]] -> database;
[[at 25]] -> lastgoals;

/*
 COMPLETE PLAN IS:
     [move from 1 to 2]
     [move from 2 to 3]
     [move from 3 to 4]
     [move from 4 to 9]
     [move from 9 to 8]
     [move from 8 to 7]
     [move from 7 to 12]
     [move from 12 to 11]
     [move from 11 to 16]
     [move from 16 to 17]
     [move from 17 to 22]
     [move from 22 to 23]
     [move from 23 to 24]
     [move from 24 to 19]
     [move from 19 to 20]
     [move from 20 to 25]
*/

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
