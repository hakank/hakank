/*
  
   3 jugs problem (a.k.a. water buckets problem) in Pop-11.

   """
   Given an 8 pint bucket of water, and two empty buckets which can contain 
   5 and 3 pints respectively, the problem requires to divide the water into 
   two by pouring water between buckets (that is, to end up with 4 pints 
   in the 8 pint bucket, and 4 pints in the 5 pint bucket) in the smallest 
   number of transfers.
   """

   This program uses the library solver to solve the 3 jugs problem.
   This version uses the known states as a graph to search.

   For more about solver, see 
     * TEACH SOLVER
   which uses the block world as an example.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');
uses teaching;
lib solver;

/*
One solution: 
8 0 0
3 5 0
8 0 0
3 5 0
3 2 3
6 2 0
6 0 2
1 5 2
1 4 3
4 4 0


   [8_bucket 5_bucket 3_bucket]

   [8 0 0] -> [8 5 0]
   [8 0 0] -> [8 0 3]
   [8 0 3] -> [8 3 0]
   [8 0 3] -> [3 5 3]
   [8 5 0] -> [8 2 3]
   [8 5 0] -> [5 5 3]





*/

vars graph;
[[1 2] [2 3] [3 4] [4 9] [9 14] [9 8] [8 7] [7 12] [12 13]
 [12 11] [11 6] [11 16] [16 17] [17 22] [21 22] [22 23]
 [23 18] [23 24] [24 19] [19 20] [20 15] [15 10] [10 5] [20 25]] -> graph;

;;;
;;; create the schemalist
;;;
vars m, Here, There;
[%for m in graph do 
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
